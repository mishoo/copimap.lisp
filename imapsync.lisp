;;;; imapsync.lisp

(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass imap ()
  ((host :initarg :host :accessor imap-host)
   (user :initarg :user :accessor imap-user)
   (password :initarg :password :accessor imap-password)
   (port :initarg :port :accessor imap-port :initform nil)
   (use-ssl :initarg :use-ssl :accessor imap-use-ssl :initform :STARTTLS)
   (capability :accessor imap-capability)
   (sock :accessor imap-sock)
   (bin-stream :accessor imap-bin-stream)
   (text-stream :accessor imap-text-stream)
   (cmdseq :initform 0 :accessor imap-cmdseq)
   (cmdqueue :initform (make-hash-table) :accessor imap-cmdqueue)
   (sock-lock :accessor imap-sock-lock)
   (thread :accessor imap-thread)
   (running :initform nil :accessor imap-running)
   (idling :initform nil :accessor imap-idling)))

(defmethod initialize-instance :after ((imap imap) &key &allow-other-keys)
  (unless (imap-port imap)
    (setf (imap-port imap)
          (if (eq t (imap-use-ssl imap))
              993
              143))))

(defgeneric imap-connect (imap &optional handler))
(defgeneric imap-close (imap))
(defgeneric imap-parse (imap))
(defgeneric imap-read-loop (imap))
(defgeneric imap-handle-ok (imap arg))
(defgeneric imap-handle-bye (imap arg))
(defgeneric imap-command (imap cmdstr &optional handler))
(defgeneric imap-has-capability (imap capability))
(defgeneric imap-write (imap str))

(defmethod imap-has-capability ((conn imap) (cap string))
  (find cap (imap-capability conn) :test #'equal))

(defmethod imap-has-capability ((conn imap) (cap symbol))
  (imap-has-capability conn (symbol-name cap)))

(defmethod imap-write ((conn imap) (data string))
  (bt2:with-lock-held ((imap-sock-lock conn))
    (let ((stream (imap-text-stream conn)))
      (write-line data stream)
      (force-output stream))))

(defun imap-new-request-id (conn handler)
  (let ((reqid (format nil "A~D" (incf (imap-cmdseq conn)))))
    (when handler
      (setf (gethash (intern reqid +atoms-package+)
                     (imap-cmdqueue conn))
            handler))
    reqid))

(defmethod imap-command ((conn imap) (cmdstr string) &optional handler)
  (format t "*** SENDING: «~A»~%" cmdstr)
  (let ((reqid (imap-new-request-id conn handler))
        (stream (imap-text-stream conn)))
    (bt2:with-lock-held ((imap-sock-lock conn))
      (write-string reqid stream)
      (write-char #\Space stream)
      (write-line cmdstr stream)
      (force-output stream))))

(defmethod imap-command ((conn imap) (cmd symbol) &optional handler)
  (imap-command conn (symbol-name cmd) handler))

(defmethod imap-command ((conn imap) (cmd cons)
                         &optional handler
                         &aux (stream (imap-text-stream conn)))
  (bt2:with-lock-held ((imap-sock-lock conn))
    (format t "*** SENDING: «~A»~%" cmd)
    (labels
        ((write-tok (tok)
           (etypecase tok
             (null
              (write-string "()" stream))
             (symbol
              (write-string (symbol-name tok) stream))
             (string
              (cond
                ((rx:scan "[\"\\n\\r]" tok)
                 (let ((bytes (trivial-utf-8:string-to-utf-8-bytes tok)))
                   (cond
                     ((or (<= (length bytes) 4096)
                          (imap-has-capability conn :LITERAL+))
                      (format stream "{~D+}~%" (length bytes))
                      (write-sequence bytes (imap-bin-stream conn)))
                     (t
                      ;; I guess that must be a really old IMAP server...
                      (error "Synchronizing literal not implemented")
                      ;; (format stream "{~D}~%" (length bytes))
                      ))))
                (t
                 (format stream "\"~A\"" tok))))
             (integer
              (format stream "~D" tok))
             (cons
              (cond
                ((eq :seq (car tok))
                 (write-tok (cadr tok))
                 (write-char #\: stream)
                 (write-tok (caddr tok)))
                (t
                 (write-char #\( stream)
                 (loop for first = t then nil
                       for tok in tok
                       do (unless first
                            (write-char #\Space stream))
                          (write-tok tok))
                 (write-char #\) stream))))
             ((vector (unsigned-byte 8))
              (unless (imap-has-capability conn :BINARY)
                (error "IMAP server is missing capability: BINARY"))
              (format stream "~~{~D+}~%" (length tok))
              (write-sequence tok (imap-bin-stream conn))))))
      (write-string (imap-new-request-id conn handler) stream)
      (loop for tok in cmd do
        (write-char #\Space stream)
        (write-tok tok)))
    (format stream "~%")
    (force-output stream)))

(defmethod imap-parse ((conn imap))
  (let* ((line (bt2:with-lock-held ((imap-sock-lock conn))
                 (%read-cmdline (imap-text-stream conn)))))
    (format t "--- GOT: ~A~%" line)
    (cond
      ((eq (car line) :untagged)
       (cond
         ((eq (cadr line) '$OK)
          (imap-handle-ok conn (caddr line)))
         ((eq (cadr line) '$BYE)
          (imap-handle-bye conn (caddr line)))))
      ((eq (car line) :continue)
       :continue)
      (t
       (let* ((id (car line))
              (handler (gethash id (imap-cmdqueue conn))))
         (when handler
           (funcall handler (cdr line))
           (remhash id (imap-cmdqueue conn))))))))

(defmethod imap-handle-ok ((conn imap) arg)
  (when (listp arg)
    (case (car arg)
      ($CAPABILITY
       (setf (imap-capability conn)
             (mapcar (lambda (x)
                       (if (stringp x)
                           (string-upcase x)
                           (symbol-name x)))
                     (cdr arg)))))))

(defmethod imap-handle-bye ((conn imap) arg)
  (format t "Logged out~%")
  (imap-close conn))

(defmethod imap-connect ((conn imap) &optional connected-handler)
  (let* ((sock (sock:socket-connect (imap-host conn)
                                    (imap-port conn)
                                    :element-type '(unsigned-byte 8)))
         (tname (format nil "IMAP:~A/~A" (imap-host conn) (imap-user conn))))
    (setf (imap-sock conn) sock
          (imap-sock-lock conn) (bt2:make-lock :name tname))

    (labels
        ((login ()
           (imap-command
            conn `(:LOGIN ,(imap-user conn) ,(imap-password conn))
            (lambda (args)
              (when (eq '$OK (car args))
                (imap-handle-ok conn (cadr args))
                (format t "CAPAB: ~S~%" (imap-capability conn))
                (setf (imap-thread conn)
                      (bt2:make-thread (lambda ()
                                         (when connected-handler
                                           (funcall connected-handler))
                                         (setf (imap-running conn) t)
                                         (imap-read-loop conn))
                                       :name tname)))))
           (imap-parse conn))

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

(defmethod imap-close ((conn imap))
  (when (imap-sock conn)
    (sock:socket-close (imap-sock conn)))
  (setf (imap-running conn) nil
        (imap-sock conn) nil
        (imap-bin-stream conn) nil
        (imap-text-stream conn) nil
        (imap-thread conn) nil))

(defmethod imap-read-loop ((conn imap))
  (format t "Starting read loop (~A)~%" (imap-host conn))
  (loop with sock = (imap-sock conn)
        with stream = (imap-text-stream conn)
        while (imap-running conn)
        do (sock:wait-for-input sock :timeout 1)
           (loop while (and (imap-running conn)
                            (listen stream))
                 do (imap-parse conn)))
  (format t "Loop terminated (~A)~%" (imap-host conn)))
