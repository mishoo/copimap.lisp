(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass imap ()
  ((host :initarg :host :accessor imap-host
         :documentation "IMAP server hostname")

   (user :initarg :user :accessor imap-user
         :documentation "IMAP username")

   (password :initarg :password :accessor imap-password
             :documentation "IMAP user password")

   (use-ssl :initarg :use-ssl :accessor imap-use-ssl :initform t
            :documentation "Pass `T' to use SSL (default), `:STARTTLS' or
`:nope-just-send-my-password-in-clear-text'. Note that for `:STARTTLS'
we require the server to advertise the `STARTTLS' capability; login
will not be attempted over plain text.")

   (port :initarg :port :accessor imap-port :initform nil
         :documentation "Server port. Will default to 143 or 993, depending on the value of `use-ssl'.")

   (capability :accessor imap-capability
               :documentation "Filled from the server (list of uppercase strings).")

   (sock :initform nil :accessor imap-sock
         :documentation "The main connection, initialized in `imap-connect' (`usocket')")

   (bin-stream :accessor imap-bin-stream
               :documentation "The binary stream, initialized in `imap-connect'")

   (text-stream :accessor imap-text-stream
                :documentation "The text stream (a `flexi-stream')")

   (cmdseq :initform 0 :accessor imap-cmdseq
           :documentation "Current request id, incremented on each `imap-command'.")

   (cmdqueue :initform (make-hash-table) :accessor imap-cmdqueue
             :documentation "Handlers for commands awaiting replies.")

   (sock-lock :accessor imap-sock-lock
              :documentation "Mutex that will be held on reading/writing from the streams.")

   (thread :accessor imap-thread
           :documentation "The read loop thread.")

   (running :initform nil :accessor imap-running
            :documentation "The read loop will continue as long as this remains `T'.")

   (reconnect :initform t :accessor imap-reconnect
              :documentation "Wether to attempt reconnecting when the socket is closed. See
`imap-maybe-reconnect'."))

  (:documentation "Low-level IMAP class.

Supports plain authentication, sending commands and receiving output.

IMAP is a fully asynchronous protocol, in that when you request some
data, the server will send results in the form of untagged
notifications that cannot be reliably linked to your request (with a
few exceptions). The `imap-connect' method will start a thread that
continuously parses output from the server and calls `imap-handle' for
any untagged notifications. You can specialize this method in
subclasses to make sense of this data. The base methods just log
output via the `verbose' package."))

(defmethod initialize-instance :after ((imap imap) &key &allow-other-keys)
  (unless (imap-port imap)
    (setf (imap-port imap)
          (if (eq t (imap-use-ssl imap))
              993
              143))))

(defgeneric imap-connect (imap))
(defgeneric imap-close (imap))
(defgeneric imap-parse (imap))
(defgeneric imap-read-loop (imap))
(defgeneric imap-handle (imap cmd arg))
(defgeneric imap-command (imap cmdstr &optional handler))
(defgeneric imap-has-capability (imap capability))
(defgeneric imap-write (imap str))
(defgeneric imap-maybe-reconnect (imap))

(defgeneric imap-on-connect (imap))

(defmethod imap-on-connect ((conn imap))
  "Will be invoked after successful authentication. Could be useful in
subclasses, for example to re-SELECT the appropriate mailbox.")

(defmethod imap-has-capability ((conn imap) (cap string))
  (find cap (imap-capability conn) :test #'equal))

(defmethod imap-has-capability ((conn imap) (cap symbol))
  (imap-has-capability conn (symbol-name cap)))

(defun imap-set-capability (conn arg)
  (setf (imap-capability conn)
        (mapcar (lambda (x)
                  (if (stringp x)
                      (string-upcase x)
                      (symbol-name x)))
                arg)))

(defmethod imap-write ((conn imap) (data string))
  (imap-maybe-reconnect conn)
  (bt2:with-recursive-lock-held ((imap-sock-lock conn))
    (let ((stream (imap-text-stream conn)))
      (write-line data stream)
      (force-output stream))))

(defun imap-new-request-id (conn handler)
  (let ((reqid (format nil "A~D" (incf (imap-cmdseq conn)))))
    (when handler
      (setf (gethash (intern reqid +atoms-package+)
                     (imap-cmdqueue conn))
            (list handler)))
    reqid))

(defmethod imap-command ((conn imap) (cmdstr string) &optional handler)
  "Send a raw command. Use this for simple commands, or if you know what
you're doing."
  (imap-maybe-reconnect conn)
  (v:debug :send "~A" cmdstr)
  (let ((reqid (imap-new-request-id conn handler))
        (stream (imap-text-stream conn)))
    (bt2:with-recursive-lock-held ((imap-sock-lock conn))
      (write-string reqid stream)
      (write-char #\Space stream)
      (write-line cmdstr stream)
      (force-output stream))
    reqid))

(defmethod imap-command ((conn imap) (cmd symbol) &optional handler)
  (imap-command conn (symbol-name cmd) handler))

(defmethod imap-command ((conn imap) (cmd cons)
                         &optional handler
                         &aux (stream (progn
                                        (imap-maybe-reconnect conn)
                                        (imap-text-stream conn))))
  "Send a command to the server. The command will be assembled from
`cmd', which should be a list. A few examples:

    (imap-command imap '(:list \"\" \"*\"))
    (imap-command imap '(:select inbox))
    (imap-command imap '(:uid :fetch 1 (flags envelope)))
    (imap-command imap '(:uid :fetch 1 (flags envelope (:binary.peek 1))))

Symbols will be sent as IMAP atoms; they don't have to be keywords,
but some keywords are treated specially at the start of a list:

    (:range 1 5) -> 1:5
    (:range 10 :*) -> 10:*
    (:seq 1 3 5 6) -> 1,3,5,6

You can mix :seq and :range:

    (:seq 1 (:range 5 10) 15) -> 1,5:10,15

The following are converted to the form BODY[1.1], used in FETCH
commands; examples:

    (:body) -> BODY[]
    (:body 1 1) -> BODY[1.1]
    (:binary 1) -> BINARY[1]
    (:body.peek ...)
    (:binary.peek ...)
    (:binary.size ...)

Peculiarities of the protocol require us to write these arguments one
by one, rather than assemble a full command in a string and write it
at once. For example, some data may be binary (e.g. if you pass
a (vector (unsigned-byte 8))), and they require writing to the binary
stream. Or, depending on server capability (or lack thereof) we might
need to send synchronizing literals
(https://www.rfc-editor.org/rfc/rfc9051#name-string) which requires us
to wait for a continuation request from the server. The mutex will be
held until we encode the whole command and force output.

If you pass `handler', it must be a function of at least one argument,
but more can be sent for specific commands (e.g. SEARCH), so your
handler should be prepared to accept more than one argument. The
handler will be called from the read loop thread when the server sends
us a tagged response which includes the current request ID. The
argument will be a list where the first item will be $OK (symbol OK in
the `+atoms-package+') if the command was successful.

Various data requested by your command may come before your handler is
called, via untagged notifications, for which the read loop will
invoke `imap-handle'."
  (bt2:with-recursive-lock-held ((imap-sock-lock conn))
    (v:debug :send "~S" cmd)
    (labels
        ((write-delimited (sep list)
           (loop for first = t then nil
                 for i in list
                 do (unless first (write-char sep stream))
                    (write-tok i)))
         (write-tok (tok)
           (etypecase tok
             (null
              (write-string "()" stream))
             (symbol
              (write-string (symbol-name tok) stream))
             (string
              (cond
                ((rx:scan "[\"\\n\\r\\x00]" tok)
                 (let ((bytes (trivial-utf-8:string-to-utf-8-bytes tok)))
                   (cond
                     ((or (<= (length bytes) 4096)
                          (imap-has-capability conn :LITERAL+))
                      (format stream "{~D+}~%" (length bytes))
                      (force-output stream)
                      (write-sequence bytes (imap-bin-stream conn))
                      (force-output (imap-bin-stream conn)))
                     (t
                      (format stream "{~D}~%" (length bytes))
                      (force-output stream)
                      (loop until (eq :continue (imap-parse conn)))
                      (write-sequence bytes (imap-bin-stream conn))
                      (force-output (imap-bin-stream conn))))))
                (t
                 (write-char #\" stream)
                 (write-string tok stream)
                 (write-char #\" stream))))
             (integer
              (format stream "~D" tok))
             (cons
              (case (car tok)
                (:range
                 (write-tok (cadr tok))
                 (write-char #\: stream)
                 (write-tok (caddr tok)))
                (:seq
                 (write-delimited #\, (cdr tok)))
                ((:body :binary :body.peek :binary.peek :binary.size)
                 (write-string (symbol-name (car tok)) stream)
                 (write-char #\[ stream)
                 (write-delimited #\. (cdr tok))
                 (write-char #\] stream))
                (t
                 (write-char #\( stream)
                 (write-delimited #\Space tok)
                 (write-char #\) stream))))
             ((vector (unsigned-byte 8))
              (unless (imap-has-capability conn :BINARY)
                (error "IMAP server is missing capability: BINARY"))
              (format stream "~~{~D+}~%" (length tok))
              (force-output stream)
              (write-sequence tok (imap-bin-stream conn))
              (force-output (imap-bin-stream conn))))))
      (let ((reqid (imap-new-request-id conn handler)))
        (write-string reqid stream)
        (write-char #\Space stream)
        (write-delimited #\Space cmd)
        (write-char #\Newline stream)
        (force-output stream)
        reqid))))

(defmethod imap-parse ((conn imap))
  "Parse one command line from the server. Holds the mutex while the
command is parsed. Invokes `imap-handle' for untagged
notifications. On tagged responses it invokes the associated handler
from `imap-cmdqueue', if found."
  (let* ((line (bt2:with-recursive-lock-held ((imap-sock-lock conn))
                 (%read-cmdline (imap-text-stream conn)))))
    (v:debug :in "~A" line)
    (cond
      ((eq (car line) :untagged)
       (if (numberp (cadr line))
           ;; FETCH responses are prefixed with the message number
           (imap-handle conn (caddr line) (list* (cadr line) (cdddr line)))
           ;; other notifications
           (imap-handle conn (cadr line) (cddr line))))
      ((eq (car line) :continue)
       :continue)
      (t
       (let* ((id (car line))
              (handler (gethash id (imap-cmdqueue conn))))
         (when handler
           (apply (car handler) (cdr line) (cdr handler))
           (remhash id (imap-cmdqueue conn))))))))

(defmethod imap-handle ((conn imap) (cmd (eql '$OK)) arg)
  "The default handler for untagged OK notifications. Sets
`imap-capability' if CAPABILITY is present in the argument (Dovecot
does that)."
  (when (listp (car arg))
    (case (caar arg)
      ($CAPABILITY
       (imap-set-capability conn (cdar arg))))))

(defmethod imap-handle ((conn imap) (cmd (eql '$CAPABILITY)) arg)
  (imap-set-capability conn arg))

(defmethod imap-handle ((conn imap) (cmd (eql '$NO)) arg)
  (v:warn :imap "Got untagged NO: ~A" arg))

(defmethod imap-handle ((conn imap) (cmd (eql '$BYE)) arg)
  "The BYE handler will close the socket."
  (v:warn :imap "Logged out: ~A" arg)
  (imap-close conn))

(defmethod imap-handle ((conn imap) (cmd (eql '$ESEARCH)) arg)
  "The ESEARCH handler collects the argument, so that it can be passed to
the handler when the command is done.

https://www.ietf.org/rfc/rfc9051.html#name-esearch-response"
  (when (and (listp (car arg))
             (eq '$TAG (caar arg)))
    (let ((reqid (cadar arg)))
      (when (stringp reqid)
        (setf reqid (intern reqid +atoms-package+)))
      (let ((handler (gethash reqid (imap-cmdqueue conn))))
        (if handler
            (push (cdr arg) (cdr handler))
            (v:warn :imap "REQID ~A from ESEARCH not found" reqid))))))

(defmethod imap-handle ((conn imap) cmd arg)
  (v:debug :imap "[~A] ~A" cmd arg))

(defmethod imap-connect ((conn imap))
  (let* ((sock (sock:socket-connect (imap-host conn)
                                    (imap-port conn)
                                    :element-type '(unsigned-byte 8)))
         (tname (format nil "IMAP:~A/~A" (imap-host conn) (imap-user conn))))
    (setf (imap-sock conn) sock
          (imap-sock-lock conn) (bt2:make-recursive-lock :name tname))
    (labels
        ((login ()
           (setf (imap-thread conn)
                 (bt2:make-thread (lambda ()
                                    (setf (imap-running conn) t)
                                    (imap-read-loop conn))
                                  :name tname))
           (imap-command
            conn `(:LOGIN ,(imap-user conn) ,(imap-password conn))
            (lambda (args)
              (when (eq '$OK (car args))
                (imap-handle conn '$OK (cdr args))
                (v:debug :imap "CAPAB: ~A" (imap-capability conn))
                (imap-on-connect conn)))))
         (setup-ssl ()
           (setf (imap-bin-stream conn)
                 (cl+ssl:make-ssl-client-stream (sock:socket-stream sock) :verify nil))
           (setf (imap-text-stream conn)
                 (flex:make-flexi-stream (imap-bin-stream conn)
                                         :external-format '(:utf-8 :eol-style :crlf)))))
      (ecase (imap-use-ssl conn)
        (:STARTTLS
         (setf (imap-text-stream conn)
               (flex:make-flexi-stream (sock:socket-stream sock)
                                       :external-format '(:utf-8 :eol-style :crlf)))
         (imap-parse conn)
         (unless (imap-has-capability conn :STARTTLS)
           (error "IMAP server is missing capability: STARTTLS"))
         (imap-command conn :STARTTLS
                       (lambda (args)
                         (declare (ignore args))
                         (setup-ssl)
                         (login)))
         (imap-parse conn))
        ((t)
         (setup-ssl)
         (imap-parse conn)
         (login))
        (:nope-just-send-my-password-in-clear-text
         (setf (imap-bin-stream conn)
               (flex:make-flexi-stream (sock:socket-stream sock)))
         (setf (imap-text-stream conn)
               (flex:make-flexi-stream (sock:socket-stream sock)
                                       :external-format '(:utf-8 :eol-style :crlf)))
         (imap-parse conn)
         (login))))))

(defmethod imap-maybe-reconnect ((conn imap))
  (unless (imap-sock conn)
    (when (imap-reconnect conn)
      (imap-connect conn))))

(defmethod imap-close ((conn imap))
  (when (imap-sock conn)
    (sock:socket-close (imap-sock conn)))
  (setf (imap-running conn) nil
        (imap-sock conn) nil
        (imap-bin-stream conn) nil
        (imap-text-stream conn) nil
        (imap-thread conn) nil))

(defmethod imap-read-loop ((conn imap))
  (v:debug :imap "Starting read loop (~A)" (imap-host conn))
  (loop with sock = (imap-sock conn)
        with stream = (imap-text-stream conn)
        while (imap-running conn)
        do (sock:wait-for-input sock :timeout 1)
           (loop while (and (imap-running conn)
                            (listen stream))
                 do (imap-parse conn)))
  (v:debug :imap "Loop terminated (~A)" (imap-host conn)))
