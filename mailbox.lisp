(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass mailbox ()
  ((mailbox-name :initarg :mailbox-name :accessor mailbox-name)
   (permanent-flags :initform nil :initarg :permanent-flags :accessor mailbox-permanent-flags)
   (flags :initform nil :initarg :flags :accessor mailbox-flags)
   (exists :initform 0 :initarg :exists :accessor mailbox-exists)
   (recent :initform 0 :initarg :recent :accessor mailbox-recent)
   (unseen :initform 0 :initarg :unseen :accessor mailbox-unseen)
   (uidvalidity :initform nil :initarg :uidvalidity :accessor mailbox-uidvalidity)
   (uidnext :initform 0 :initarg :uidnext :accessor mailbox-uidnext)
   (highestmodseq :initform 0 :initarg :highestmodseq :accessor mailbox-highestmodseq)))

(defclass imap+mailbox (imap mailbox)
  ((local-store :initarg :local-store :accessor mailbox-local-store)
   (gmail :initform nil :accessor mailbox-gmail)))

(defgeneric mailbox-fetch (mailbox uids &optional handler))

(defmacro with-local-store (conn &body body)
  `(with-slots ((store local-store)) ,conn
     (when store
       ,@body)))

(defmethod mailbox-local-store-directory ((conn imap+mailbox))
  (format nil "~~/Imapsync/~A/~A/~A/"
          (imap-host conn)
          (imap-user conn)
          (rx:regex-replace-all "[\\[\\]]" (mailbox-name conn) "_")))

(defmethod initialize-instance :after ((conn imap+mailbox) &key &allow-other-keys)
  (when (and (slot-boundp conn 'local-store)
             (stringp (mailbox-local-store conn)))
    (setf (mailbox-local-store conn)
          (make-instance 'maildir-store
                         :path (mailbox-local-store conn))))
  (unless (slot-boundp conn 'local-store)
    (setf (mailbox-local-store conn)
          (make-instance 'maildir-store
                         :path (mailbox-local-store-directory conn)))))

(defmethod imap-on-connect ((conn imap+mailbox))
  (when (imap-has-capability conn :X-GM-EXT-1)
    (setf (mailbox-gmail conn) t)
    (imap-command conn '(enable :X-GM-EXT-1))
    ;; (imap-command conn '(enable :UTF8=ACCEPT))
    )
  (imap-command conn `(:select (:astr ,(mailbox-name conn)))
                (lambda (arg)
                  (when-ok arg
                    (v:info :mailbox "Selected mailbox ~A/~A" (mailbox-name conn) (caar arg))))))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$OK)) arg)
  (when (listp (car arg))
    (let ((arg (car arg)))
      (case (car arg)
        ($PERMANENTFLAGS (setf (mailbox-permanent-flags conn) (cadr arg)))
        ($UNSEEN (setf (mailbox-unseen conn) (cadr arg)))
        ($UIDVALIDITY (setf (mailbox-uidvalidity conn) (cadr arg)))
        ($UIDNEXT (setf (mailbox-uidnext conn) (cadr arg)))
        ($HIGHESTMODSEQ (setf (mailbox-highestmodseq conn) (cadr arg)))
        (t (call-next-method))))))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$FLAGS)) arg)
  (setf (mailbox-flags conn) (car arg)))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$EXISTS)) arg)
  (setf (mailbox-exists conn) (car arg))
  (mailbox-fetch-new conn))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$RECENT)) arg)
  (setf (mailbox-recent conn) (car arg)))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$FETCH)) arg)
  (with-local-store conn
    (store-save-messages store conn (list (cadr arg)))))

(defmethod mailbox-fetch ((conn imap+mailbox) uids &optional handler)
  (with-idle-resume conn
    (imap-command conn
                  `(UID FETCH ,uids
                        (UID
                         INTERNALDATE
                         ENVELOPE
                         FLAGS
                         ,@(when (mailbox-gmail conn) '(X-GM-LABELS))
                         (:BODY.PEEK)))
                  handler)))

(defmethod mailbox-fetch-new ((conn imap+mailbox) &optional handler)
  (with-local-store conn
    (let ((uid (or (store-get-last-uid store) 0)))
      (with-idle-resume conn
        (imap-command conn `(uid search return (max count all) uid (:range ,(1+ uid) *))
                      (lambda (arg &optional ret)
                        (when-ok arg
                          (v:debug :esearch "~S" ret)
                          (let ((max (getf (cdr ret) '$MAX 0))
                                (count (getf (cdr ret) '$COUNT 0))
                                (new-uids (getf (cdr ret) '$ALL)))
                            (cond
                              ((< uid max)
                               (v:info :sync "Fetching ~D new messages" count)
                               (mailbox-fetch conn new-uids handler))
                              (t
                               (v:info :sync "No new messages")))))))))))

(defmethod mailbox-push-local-changes ((conn imap+mailbox))
  (with-local-store conn
    (let* ((changes (store-find-local-changes store))
           (cmdcount 0)
           (handler (lambda (arg)
                      (when-ok arg
                        (when (zerop (decf cmdcount))
                          (store-update-for-changes store changes)
                          (v:debug :push "SUCCESS pushing local changes"))))))
      (loop for (uid . data) in changes
            for setcmd = nil
            do (if (eq uid 'expunged)
                   (cond
                     ((mailbox-gmail conn)
                      ;; for Gmail it seems the proper way to remove
                      ;; messages is to add the \Trash label.
                      (incf cmdcount)
                      (imap-command imap `(UID STORE (:seq . ,data) +X-GM-LABELS.SILENT ($\\Trash))
                                    handler))
                     ((imap-has-capability conn :UIDPLUS)
                      (incf cmdcount)
                      (imap-command imap `(UID STORE (:seq . ,data) +FLAGS.SILENT ($\\Deleted))
                                    (lambda (arg)
                                      (declare (ignore arg))
                                      (imap-command imap `(UID EXPUNGE (:seq . ,data)) handler))))
                     (t
                      (v:warn :expunge "Cannot expunge ~A (missing UIDPLUS)" data)))
                   (destructuring-bind (&key +label -label +flags -flags &allow-other-keys) data
                     (when (and +label (mailbox-gmail conn))
                       (setf setcmd `(+X-GM-LABELS.SILENT (:astr . ,+label) ,@setcmd)))
                     (when (and -label (mailbox-gmail conn))
                       (setf setcmd `(-X-GM-LABELS.SILENT (:astr . ,-label) ,@setcmd)))
                     (when +flags
                       (setf setcmd `(+FLAGS.SILENT (:astr . ,+flags) ,@setcmd)))
                     (when -flags
                       (setf setcmd `(-FLAGS.SILENT (:astr . ,-flags) ,@setcmd)))
                     (when setcmd
                       (incf cmdcount)
                       (imap-command conn `(UID STORE ,uid . ,setcmd) handler))))))))
