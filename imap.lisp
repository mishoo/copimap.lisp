(in-package #:copimap)

(in-readtable copimap::syntax)

(defclass imap ()
  ((host :initarg :host :accessor imap-host
         :documentation "IMAP server hostname")

   (user :initarg :user :accessor imap-user
         :documentation "IMAP username")

   (password :initarg :password :accessor imap-password
             :documentation "IMAP user password. Pass either the raw password as string, or a list
to invoke an external program (should write the password to standard
output). External programs are run via `uiop:run-program'. If the form
is (:shell \"command\") then we'll pass :force-shell T.")

   (use-ssl :initarg :use-ssl :accessor imap-use-ssl :initform t
            :documentation "Pass T to use SSL (default), `:STARTTLS' or
`:nope-just-send-my-password-in-clear-text'. Note that for `:STARTTLS'
we require the server to advertise the `STARTTLS' capability; login
will not be attempted over plain text.")

   (port :initarg :port :accessor imap-port :initform nil
         :documentation "Server port. Will default to 143 or 993, depending on the value of `use-ssl'.")

   (capability :reader imap-capability
               :documentation "Filled from the server (list of uppercase strings).")

   (sock :initform nil :accessor imap-sock
         :documentation "The main connection, initialized in `imap-connect' (`usocket')")

   (bin-stream :accessor imap-bin-stream
               :documentation "The binary stream, initialized in `imap-connect'")

   (text-stream :accessor imap-text-stream
                :documentation "The text stream (a `flexi-stream')")

   (cmdseq :initform 0 :accessor imap-cmdseq
           :documentation "Current request id, incremented on each `imap-command'.")

   (cmdqueue :initform (make-hash-table :test 'equal) :accessor imap-cmdqueue
             :documentation "Handlers for commands awaiting replies.")

   (sock-lock :accessor imap-sock-lock
              :documentation "Mutex that will be held on reading/writing from the streams.")

   (thread :accessor imap-thread
           :documentation "The read loop thread.")

   (running :initform nil :accessor imap-running
            :documentation "The read loop will continue as long as this remains T.")

   (reconnect :initform t :accessor imap-reconnect
              :documentation "Wether to attempt reconnecting when the socket is closed. See
`imap-maybe-reconnect'.")

   (idling :initform nil :accessor imap-idling
           :documentation "Used internally to keep track of wether an IDLE command is in
progress. See `imap-start-idle'.")

   (poll-time :initform 1200 :initarg :poll-time :accessor imap-poll-time
              :documentation "If non-NIL, the read loop will send NOOP commands every so many
seconds. Default is 20 minutes.")

   (last-command-time :initform nil :accessor imap-last-command-time
                      :documentation "Used internally for polling (keeps track of last command time).")

   (utf-7 :initform nil :reader imap-utf-7
          :documentation "Whether we should do mUTF-7 conversion for ASTRING-s. Will be set when
we receive capabilities if IMAP4rev1 is advertised."))

  (:documentation "Low-level IMAP class.

Supports plain authentication, sending commands and receiving output.

IMAP is a fully asynchronous protocol, in that when you request some
data, the server will send results in the form of untagged
notifications that can't always be reliably linked to your
request. The `imap-connect' method will start a thread that
continuously parses input from the server via `imap-parse', which
see. Use `imap-command' to send commands to the IMAP server, and
specialize `imap-handle' in your own subclasses in order to retrieve
results from the server."))

(defgeneric imap-connect (imap)
  (:documentation "
Starts the IMAP connection. After successful authentication the read
loop thread is started. Returns T on success."))

(defgeneric imap-close (imap)
  (:documentation "Close the connection"))

(defgeneric imap-parse (imap)
  (:documentation "
Parse one command line from the server. Invokes `imap-handle' for
untagged notifications. On tagged responses it invokes the associated
handler from `imap-cmdqueue', if found.

The command line, as returned by `parse-imap-cmdline', is a list of
tokens (similar to the ones that you send to `imap-command'). The
first is `:untagged' for notifications or `:continue' for continuation
requests, otherwise it's a request ID followed by return value.

IMAP atoms are similar to Lisp symbols, so they are parsed as such,
and interned into `+atoms-package+' (COPIMAP-ATOMS). Note that the
reader syntax in `COPIMAP' defines a custom reader for $ which interns
symbols in `+atoms-package+'. This reader is case sensitive, so symbol
`$OK' will be different from `$ok'."))

(defgeneric imap-handle (imap cmd arg)
  (:documentation "
Will be invoked by `imap-parse' for untagged notification. `cmd' is
the notification label as a copimap symbol (e.g. $FETCH, $EXISTS etc.)
and `arg' contains whatever arguments were sent."))

(defgeneric imap-command (imap cmd &optional handler)
  (:documentation "
Send a command to the IMAP server. The function returns immediately,
and `handler' will be invoked from the read thread when a tagged
response comes back. See the documentation of the available methods;
the most useful one is where `cmd' is a list."))

(defgeneric imap-command-sync (imap cmdstr &optional handler)
  (:documentation "
Synchronously execute command. This holds the mutex and parses server
output until a tagged response for our command comes back (at which
point `handler' will be invoked with the argument). Note that you
cannot use this for the IDLE command (there won't be a tagged answer
until you send DONE)."))

(defgeneric imap-start-idle (imap &optional handler)
  (:documentation "
Enter IDLE mode. While idling the server can send notifications which
will be handled by the read loop thread via `imap-parse'. Sending any
`imap-command' while a connection is idling will automatically stop
IDLE mode, and resume it back after sending the command. See also
macro `with-idle-resume'."))

(defgeneric imap-stop-idle (imap)
  (:documentation "Ends IDLE mode."))

(defgeneric imap-has-capability (imap capability)
  (:documentation "
Checks if the server advertises the `cap' capability (should be a
keyword symbol)."))

(defgeneric imap-write (imap str))

(defgeneric imap-read-loop (imap))
(defgeneric imap-start-read-loop (imap))

(defgeneric imap-on-connect (imap)
  (:documentation "
Will be invoked after successful authentication. Could be useful in
subclasses, for example to re-SELECT the appropriate mailbox."))

;;;;; implementation

(defmacro with-sock-lock (conn &body body)
  `(bt2:with-recursive-lock-held ((imap-sock-lock ,conn))
     ,@body))

(defmethod initialize-instance :after ((imap imap) &key &allow-other-keys)
  (unless (imap-port imap)
    (setf (imap-port imap)
          (if (eq t (imap-use-ssl imap))
              993
              143))))

(defmethod imap-password ((conn imap))
  (let ((pass (slot-value conn 'password)))
    (if (not (consp pass))
        pass
        (let ((shell nil))
          (when (eq :shell (car pass))
            (setf pass (cdr pass) shell t))
          (string-trim '(#\Return #\Newline)
                       (with-output-to-string (out)
                         (with-output-to-string (err)
                           (uiop:run-program pass :output out
                                                  :error-output err
                                                  :force-shell shell))))))))

(defmethod imap-on-connect ((conn imap))
  (v:debug :imap "Connected to ~A (~A)" (imap-host conn) (imap-user conn)))

(defmethod (setf imap-capability) (value (conn imap))
  (setf (slot-value conn 'capability)
        (map 'vector
             (lambda (x)
               (intern (string-upcase (as-string x)) :keyword))
             value))
  (when (imap-has-capability conn :imap4rev1)
    (setf (slot-value conn 'utf-7) t)))

(declaim (inline destr))
(defun destr (conn x)
  (if (imap-utf-7 conn)
      (typecase x
        (null x)
        (string (mutf-7-to-string x))
        (symbol (intern (mutf-7-to-string (symbol-name x))
                        (symbol-package x)))
        (t x))
      x))

(declaim (inline enstr))
(defun enstr (conn x)
  (if (imap-utf-7 conn)
      (typecase x
        (null x)
        (string (string-to-mutf-7 x))
        (symbol (intern (string-to-mutf-7 (symbol-name x))
                        (symbol-package x)))
        (t x))
      x))

(defmethod imap-has-capability ((conn imap) (cap symbol))
  (find cap (imap-capability conn)))

(defmethod imap-write ((conn imap) (data string))
  (v:trace :send "~A" data)
  (with-sock-lock conn
    (let ((stream (imap-text-stream conn)))
      (write-line data stream)
      (force-output stream))))

(defun imap-new-request-id (conn handler)
  (let ((reqid (format nil "A~D" (incf (imap-cmdseq conn)))))
    (when handler
      (setf (gethash reqid (imap-cmdqueue conn))
            (list handler)))
    reqid))

(defmacro when-ok (arg &body body)
  `(when (and (listp ,arg)
              (eq '$OK (car ,arg)))
     (setf ,arg (cdr ,arg))
     ,@body))

(defmethod imap-command ((conn imap) (cmdstr string) &optional handler)
  "Send a raw command. Use this for simple commands, or if you know what
you're doing."
  (let ((reqid (imap-new-request-id conn handler))
        (stream (imap-text-stream conn)))
    (with-sock-lock conn
      (v:debug :send "~A: ~A" reqid cmdstr)
      (write-string reqid stream)
      (write-char #\Space stream)
      (write-line cmdstr stream)
      (force-output stream))
    reqid))

(defmethod imap-command ((conn imap) (cmd symbol) &optional handler)
  (imap-command conn (symbol-name cmd) handler))

(defmethod imap-command ((conn imap) (cmd cons)
                         &optional handler
                         &aux (stream (imap-text-stream conn)))
  "Send a command to the server. The command will be assembled from
`cmd', which should be a list. A few examples:

    (imap-command imap '(:list \"\" \"*\"))
    (imap-command imap '(:select inbox))
    (imap-command imap '(:uid :fetch (:range 1 5) (flags envelope)))
    (imap-command imap '(:uid :fetch 1 (flags envelope (:binary.peek 1))))

Symbols will be sent as IMAP atoms; they don't have to be keywords,
but some keywords are treated specially:

    (:range 1 5) -> 1:5
    (:range 10 :*) -> 10:*
    (:seq 1 3 5 6) -> 1,3,5,6

You can mix :seq and :range:

    (:seq 1 (:range 5 10) 15) -> 1,5:10,15

You should wrap in (:astr ...) strings/symbols which might need to be
mUTF-7 encoded, like mailbox names or flags.

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
a (vector (unsigned-byte 8))), and this requires writing to the binary
stream. Or, depending on server capability (or lack thereof) we might
need to send synchronizing literals
(https://www.rfc-editor.org/rfc/rfc9051#name-string) which requires us
to wait for a continuation request from the server. The mutex will be
held until we encode the whole command and force output.

If you pass `handler', it must be a function of at least one argument,
but more can be sent for specific commands (e.g. SEARCH), so your
handler should be prepared to accept (but not require) more than one
argument. The handler will be called from the read loop thread when
the server sends us a tagged response which includes the current
request ID. The argument will be a list where the first item will be
`$OK' (symbol OK in the `+atoms-package+') if the command was
successful.

Various data requested by your command may come before your handler is
called, via untagged notifications, for which the read loop will
invoke `imap-handle'."
  (with-sock-lock conn
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
                ((rx:scan "[\\n\\r\\x00]" tok)
                 (let ((bytes (babel:string-to-octets tok :encoding :iso-8859-1)))
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
                 (loop for ch across tok do
                   (when (or (char= ch #\\)
                             (char= ch #\"))
                     (write-char #\\ stream))
                   (write-char ch stream))
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
                (:astr
                 (write-delimited #\Space
                                  (mapcar (lambda (x) (enstr conn x))
                                          (cdr tok))))
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
        (v:debug :send "~A: ~A"
                 reqid
                 (if (eq :login (car cmd))
                     '(:login |...|)
                     cmd))
        (write-string reqid stream)
        (write-char #\Space stream)
        (write-delimited #\Space cmd)
        (write-char #\Newline stream)
        (force-output stream)
        reqid))))

(defmacro with-idle-resume (conn &body body)
  "When the connection is idling, stop IDLE mode, execute `body' and then
restart IDLE mode. When IDLE mode is off, just execute `body'."
  (a:once-only (conn
                (thunk `(lambda () ,@body)))
    `(if (imap-idling ,conn)
         (with-sock-lock ,conn
           (if (imap-idling ,conn)
               (prog2
                   (imap-stop-idle ,conn)
                   (funcall ,thunk)
                 (imap-start-idle ,conn))
               (funcall ,thunk)))
         (funcall ,thunk))))

(defmethod imap-command :before ((conn imap) cmd &optional handler)
  (declare (ignore cmd handler))
  (imap-maybe-reconnect conn)
  (setf (imap-last-command-time conn) (get-universal-time)))

(defmethod imap-command :around ((conn imap) cmd &optional handler)
  "Wrapper around `imap-command' which turns off IDLE mode."
  (declare (ignore cmd handler))
  (with-idle-resume conn
    (call-next-method)))

(defmethod imap-start-idle ((conn imap) &optional handler
                            &aux reqid)
  (unless (imap-idling conn)
    (with-sock-lock conn
      (unless (imap-idling conn)
        (setf reqid
              (imap-command
               conn "IDLE"
               (lambda (&rest args)
                 (v:debug :idle "IDLE mode finished (~A/~A)" (imap-host conn) reqid)
                 (when (equalp reqid (imap-idling conn))
                   (setf (imap-idling conn) nil))
                 (when handler (apply handler args))))
              (imap-idling conn) reqid)))))

(defmethod imap-stop-idle ((conn imap))
  (when (imap-idling conn)
    (imap-write conn "DONE")
    (setf (imap-idling conn) nil)))

(defmethod imap-command-sync ((conn imap) cmd &optional handler)
  (with-sock-lock conn
    (let ((reqid (imap-command conn cmd handler)))
      (loop until (equal reqid (imap-parse conn))))))

(defmethod imap-parse ((conn imap))
  (let* ((line (parse-imap-cmdline (imap-text-stream conn))))
    (v:trace :input "~A" line)
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
       (let* ((reqid (car line))
              (handler (gethash reqid (imap-cmdqueue conn))))
         (when handler
           (apply (car handler) (cdr line) (cdr handler))
           (remhash reqid (imap-cmdqueue conn)))
         reqid)))))

(defmethod imap-handle ((conn imap) (cmd (eql '$OK)) arg)
  "The default handler for untagged OK notifications. Sets
`imap-capability' if CAPABILITY is present in the argument (Dovecot
does that)."
  (when (listp (car arg))
    (case (caar arg)
      ($CAPABILITY
       (setf (imap-capability conn) (cdar arg))))))

(defmethod imap-handle ((conn imap) (cmd (eql '$CAPABILITY)) arg)
  (setf (imap-capability conn) arg))

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
    (let ((reqid (as-string (cadar arg))))
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
          (imap-idling conn) nil
          (imap-sock-lock conn) (bt2:make-recursive-lock :name tname))

    (setf (usocket:socket-option sock :tcp-nodelay) t
          (usocket:socket-option sock :tcp-keepalive) t)

    ;; Without these, the socket would remain stuck for dozens of
    ;; minutes when the network goes down (or e.g. you switch wifi
    ;; networks). Since `usocket:socket-option' doesn't provide
    ;; setters for them, these are SBCL-only.
    #+sbcl
    (progn
      ;; XXX: perhaps these values are too low?
      (setf (sb-bsd-sockets:sockopt-tcp-keepidle (usocket:socket sock)) 10)
      (setf (sb-bsd-sockets:sockopt-tcp-keepintvl (usocket:socket sock)) 3)
      (setf (sb-bsd-sockets:sockopt-tcp-keepcnt (usocket:socket sock)) 3))

    (labels
        ((login (&aux authenticated)
           (imap-command-sync conn `(:LOGIN ,(imap-user conn)
                                            ,(imap-password conn))
                              (lambda (args)
                                (when-ok args
                                  (imap-handle conn '$OK args)
                                  (v:debug :imap "CAPAB: ~A" (imap-capability conn))
                                  (imap-start-read-loop conn)
                                  (setf authenticated t))))
           authenticated)
         (setup-ssl ()
           (setf (imap-bin-stream conn)
                 (cl+ssl:make-ssl-client-stream (sock:socket-stream sock) :verify nil))
           (setf (imap-text-stream conn)
                 (flex:make-flexi-stream (imap-bin-stream conn)
                                         :external-format '(:iso-8859-1 :eol-style :crlf)))))
      (ecase (imap-use-ssl conn)
        (:STARTTLS
         (setf (imap-text-stream conn)
               (flex:make-flexi-stream (sock:socket-stream sock)
                                       :external-format '(:iso-8859-1 :eol-style :crlf)))
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
                                       :external-format '(:iso-8859-1 :eol-style :crlf)))
         (imap-parse conn)
         (login))))))

(defmethod imap-maybe-reconnect ((conn imap))
  (unless (imap-sock conn)
    (when (imap-reconnect conn)
      (imap-connect conn))))

(defmethod imap-close ((conn imap))
  (when (imap-sock conn)
    (ignore-errors
     (sock:socket-close (imap-sock conn))))
  (setf (imap-running conn) nil
        (imap-sock conn) nil
        (imap-bin-stream conn) nil
        (imap-text-stream conn) nil
        (imap-thread conn) nil
        (imap-sock-lock conn) nil
        (imap-idling conn) nil
        (imap-cmdqueue conn) (make-hash-table :test 'equal)
        (imap-cmdseq conn) 0))

(defmethod imap-read-loop ((conn imap))
  (v:debug :imap "Starting read loop (~A)" (imap-host conn))
  (imap-on-connect conn)
  (labels
      ((reconnect (&aux was-idle)
         (setf was-idle (imap-idling conn))
         (imap-close conn)
         (loop with count = 0
               until (loop repeat 3
                           do (v:debug :CONNECT "Attempting reconnect ~A ~A.."
                                       (imap-host conn) (incf count))
                           when (imap-connect conn)
                             do (return t)
                           do (sleep 2)
                           finally (return nil))
               do (v:debug :CONNECT "Waiting 10 seconds")
                  (sleep 10))
         (when was-idle
           (imap-start-idle conn))))
    (handler-case
        (loop for poll-time = (imap-poll-time conn)
              while (imap-running conn)
              when (and poll-time
                        (<= poll-time (- (get-universal-time)
                                         (imap-last-command-time conn))))
                do (with-idle-resume conn
                     (imap-command conn "NOOP"))
              do (sock:wait-for-input (imap-sock conn) :timeout 1)
                 (with-sock-lock conn
                   (loop while (and (imap-running conn)
                                    (listen (imap-text-stream conn)))
                         do (imap-parse conn))))
      (sock:socket-error (ex)
        (v:error :SOCKET "ERROR ~A" ex)
        (reconnect))
      (cl+ssl::ssl-error (ex)
        (v:error :SOCKET "ERROR ~A" ex)
        (reconnect)))))

(defmethod imap-start-read-loop ((conn imap))
  (let ((tname (format nil "IMAP:~A/~A" (imap-host conn) (imap-user conn))))
    (setf (imap-thread conn)
          (bt2:make-thread (lambda ()
                             (setf (imap-running conn) t)
                             (imap-read-loop conn))
                           :name tname))))
