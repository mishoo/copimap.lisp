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
   (local-store :initform nil :initarg :local-store :accessor mailbox-local-store)))

(defclass imap+mailbox (imap mailbox)
  ())

(defmethod imap-on-connect ((conn imap+mailbox))
  (imap-command conn `(:select ,(mailbox-name conn))
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
        (t (call-next-method))))))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$FLAGS)) arg)
  (setf (mailbox-flags conn) (car arg)))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$EXISTS)) arg)
  (setf (mailbox-exists conn) (car arg)))

(defmethod imap-handle ((conn imap+mailbox) (cmd (eql '$RECENT)) arg)
  (setf (mailbox-recent conn) (car arg)))
