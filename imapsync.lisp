;;;; imapsync.lisp

(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass imap ()
  ((host :initarg :host :accessor imap-host)
   (user :initarg :user :accessor imap-user)
   (password :initarg :password :accessor imap-password)
   (port :initarg :port :accessor imap-port :initform 143)
   (use-ssl :initarg :use-ssl :accessor imap-use-ssl :initform :STARTTLS)
   (capability :accessor imap-capability)
   (sock :accessor imap-sock)
   (bin-sock :accessor imap-bin-sock)
   (text-sock :accessor imap-text-sock)
   (cmdseq :initform 0 :accessor imap-cmdseq)
   (cmdqueue :initform (make-hash-table) :accessor imap-cmdqueue)))

(defgeneric imap-connect (imap))
(defgeneric imap-close (imap))
(defgeneric imap-parse (imap))
(defgeneric imap-handle-ok (imap arg))
(defgeneric imap-handle-bye (imap arg))
(defgeneric imap-command (imap cmdstr &optional handler))
(defgeneric imap-has-capability (imap str))

(defmethod imap-has-capability ((conn imap) (cap string))
  (find cap (imap-capability conn) :test #'equal))

(defmethod imap-has-capability ((conn imap) (cap symbol))
  (imap-has-capability conn (symbol-name cap)))

(defmethod imap-command ((conn imap) (cmdstr string) &optional handler)
  (unless (zerop (length cmdstr))
    (format t "*** SENDING: «~A»~%" cmdstr))
  (let ((sock (imap-text-sock conn))
        (id (incf (imap-cmdseq conn))))
    (when handler
      (setf (gethash (intern (format nil "A~D" id) *atoms-package*)
                     (imap-cmdqueue conn))
            handler))
    (format sock "A~D" id)
    (unless (zerop (length cmdstr))
      (write-char #\Space sock)
      (write-line cmdstr sock)
      (force-output sock)
      (imap-parse conn))))

(defmethod imap-command ((conn imap) (cmd symbol) &optional handler)
  (imap-command conn (symbol-name cmd) handler))

(defmethod imap-command ((conn imap) (cmd cons) &optional handler)
  (let ((sock (imap-text-sock conn)))
    (imap-command conn "" handler)
    (format t "*** SENDING: «~S»~%" cmd)
    (labels
        ((write-tok (tok)
           (etypecase tok
             (null
              (write-string "()" sock))
             (symbol
              (write-string (symbol-name tok) sock))
             (string
              (cond
                ((rx:scan "[\"\\n\\r]" tok)
                 (let ((bytes (trivial-utf-8:string-to-utf-8-bytes tok)))
                   (cond
                     ((or (<= (length bytes) 4096)
                          (imap-has-capability conn :LITERAL+))
                      (format sock "{~D+}~%" (length bytes))
                      (write-sequence bytes (imap-bin-sock conn)))
                     (t
                      ;; I guess that must be a really old IMAP server...
                      (error "Synchronizing literal not implemented")
                      ;; (format sock "{~D}~%" (length bytes))
                      ))))
                (t
                 (format sock "\"~A\"" tok))))
             (integer
              (format sock "~D" tok))
             (cons
              (cond
                ((eq :seq (car tok))
                 (write-tok (cadr tok))
                 (write-char #\: sock)
                 (write-tok (caddr tok)))
                (t
                 (write-char #\( sock)
                 (loop for first = t then nil
                       for tok in tok
                       do (unless first
                            (write-char #\Space sock))
                          (write-tok tok))
                 (write-char #\) sock))))
             ((vector (unsigned-byte 8))
              (unless (imap-has-capability conn :BINARY)
                (error "IMAP server is missing capability: BINARY"))
              (format sock "~~{~D+}~%" (length tok))
              (write-sequence tok sock)))))
      (loop for tok in cmd do
        (write-char #\Space sock)
        (write-tok tok)))
    (format sock "~%")
    (force-output sock)
    (imap-parse conn)))

(defmethod imap-parse ((conn imap))
  (let* ((sock (imap-text-sock conn))
         (line (%read-cmdline sock)))
    (format t "--- GOT: ~S~%" line)
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
           (remhash id (imap-cmdqueue conn))))))
    (when (sock:wait-for-input (imap-sock conn) :timeout 0 :ready-only t)
      (imap-parse conn))))

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

(defmethod imap-connect ((conn imap))
  (let* ((sock (sock:socket-connect (imap-host conn)
                                    (imap-port conn)
                                    :element-type '(unsigned-byte 8)))
         (plain (flex:make-flexi-stream
                 (sock:socket-stream sock)
                 :external-format '(:utf-8 :eol-style :crlf))))
    (setf (imap-sock conn) sock
          (imap-bin-sock conn) sock
          (imap-text-sock conn) plain)
    (imap-parse conn)
    (ecase (imap-use-ssl conn)
      (:STARTTLS
       (unless (imap-has-capability conn :STARTTLS)
         (error "IMAP server is missing capability: STARTTLS"))
       (imap-command conn :STARTTLS
                     (lambda (args)
                       (declare (ignore args))
                       (setf (imap-bin-sock conn)
                             (cl+ssl:make-ssl-client-stream (sock:socket-stream sock) :verify nil))
                       (setf (imap-text-sock conn)
                             (flex:make-flexi-stream (imap-bin-sock conn)
                                                     :external-format '(:utf-8 :eol-style :crlf)))
                       (imap-command
                        conn `(:LOGIN ,(imap-user conn) ,(imap-password conn))
                        (lambda (args)
                          (when (eq '$OK (car args))
                            (imap-handle-ok conn (cadr args))
                            (format t "CAPAB: ~S~%" (imap-capability conn)))))))))))

(defmethod imap-close ((conn imap))
  (sock:socket-close (imap-sock conn)))
