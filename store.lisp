(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass store ()
  ((path :initarg :path :accessor store-path)
   (db :accessor store-db)))

(defun store-db-filename (store)
  (uiop:native-namestring
   (fad:merge-pathnames-as-file (store-path store) ".imapsync.sqlite3")))

(defun store-folder (store folder)
  (fad:merge-pathnames-as-directory (store-path store) folder))

(defgeneric store-get-last-uid (store))
(defgeneric store-save-mailbox (store mailbox))
(defgeneric store-save-messages (store imap messages))
(defgeneric store-save-flags (store flags))
(defgeneric store-save-labels (store labels))
(defgeneric store-close (store))
(defgeneric store-get-by-uid (store uid))

(defmethod initialize-instance :after ((store store) &key &allow-other-keys)
  (with-slots (path db) store
    (setf path (fad:pathname-as-directory path))
    (ensure-directories-exist path)
    (let ((db-file (store-db-filename store)))
      (unless (fad:file-exists-p db-file)
        (with-open-file (out db-file :if-does-not-exist :create)))
      (setf db (dbi:connect :sqlite3 :database-name db-file))
      (dbi:do-sql db "PRAGMA foreign_keys = ON")
      (store-upgrade-db store nil))))

(defmethod store-save-mailbox ((store store) (mailbox mailbox))
  (loop with db = (store-db store)
        with query = (dbi:prepare db "INSERT INTO mailbox (key, intval) VALUES (?, ?)
                                      ON CONFLICT(key) DO UPDATE SET intval = ?")
        for key in '(exists recent unseen uidvalidity uidnext highestmodseq)
        for val = (slot-value mailbox key)
        do (dbi:execute query (list (symbol-name key) val))))

(defmethod store-save-flags ((store store) flags)
  (let ((sql (dbi:prepare (store-db store)
                 "INSERT INTO flag (name) VALUES (?) ON CONFLICT DO NOTHING")))
    (loop for name in flags do
      (dbi:execute sql (list (as-string name))))))

(defmethod store-save-labels ((store store) labels)
  (let ((sql (dbi:prepare (store-db store)
                 "INSERT INTO label (name) VALUES (?) ON CONFLICT DO NOTHING")))
    (loop for name in labels do
      (dbi:execute sql (list (as-string name))))))

(defmethod store-get-last-uid ((store store))
  (sql-single (store-db store)
              "SELECT uid FROM message ORDER BY uid DESC LIMIT 1"))

(defmethod store-get-by-uid ((store store) uid)
  (sql-row (store-db store) "SELECT * FROM message WHERE uid = ?" uid))

(defmethod store-close ((store store))
  (dbi:disconnect (store-db store)))

;;;;;;;;;;;;;;;;

(defclass maildir-store (store)
  ())

(defmethod initialize-instance :after ((store maildir-store) &key &allow-other-keys)
  (ensure-directories-exist (store-folder store "new/"))
  (ensure-directories-exist (store-folder store "cur/"))
  (ensure-directories-exist (store-folder store "tmp/")))

(defgeneric maildir-message-filename (store uid))

#+nil (let ((prev-time 0) (id 0))
        (defmethod maildir-message-filename ((store maildir-store) uid)
          (let* ((time (get-universal-time))
                 (prefix (if (= time prev-time)
                             (format nil "~D_~D" time (incf id))
                             (format nil "~D" (setf id 0 prev-time time)))))
            (format nil "imapsync.~A.~A,U=~D."
                    prefix
                    (machine-instance)
                    uid))))

(defmethod maildir-message-filename ((store maildir-store) uid)
  (format nil "imapsync.~A,U=~D."
          (machine-instance)
          uid))

(defun %fix-strings (conn list)
  (mapcar (lambda (x)
            (as-string (destr conn x)))
          list))

(defun %add-x-keywords (body keywords)
  (cond
    (keywords
     (let (headers bodypos)
       (with-input-from-string (in body)
         (setf headers (parse-rfc822-headers in)
               bodypos (file-position in)))
       (let ((kwhead (assoc "X-Keywords" headers :test #'equalp))
             (kwvalue (format nil "~{~A~^, ~}"
                              (mapcar #'encode-header keywords))))
         (cond
           (kwhead
            (setf (cdr kwhead) kwvalue))
           (t
            (setf (cdr (last headers))
                  (list (cons "X-Keywords" kwvalue))))))
       (with-output-to-string (out)
         (write-rfc822-headers headers out)
         (write-string (subseq body bodypos) out))))
    (t
     body)))

(defmethod store-save-messages ((store maildir-store) (conn imap) messages)
  (loop with db = (store-db store)
        with insert-message = (dbi:prepare db "INSERT INTO message (uid, local_uid, path, internaldate, mtime) VALUES (?, ?, ?, ?, ?)")
        with add-flags = (dbi:prepare db "INSERT INTO map_flag_message (flag, message) VALUES (?, ?)")
        with add-labels = (dbi:prepare db "INSERT INTO map_label_message (label, message) VALUES (?, ?)")
        with mtime
        for msg in messages
        for uid = (getf msg '$UID)
        for internaldate = (let* ((date (getf msg '$INTERNALDATE))
                                  (ts (when date (parse-internaldate date))))
                             (when ts (local-time:timestamp-to-unix ts)))
        for flags = (getf msg '$FLAGS)
        for str-flags = (%fix-strings conn flags)
        for labels = (getf msg '$X-GM-LABELS)
        for str-labels = (%fix-strings conn labels)
        for seen = (find '$\\Seen flags)
        for envelope = (getf msg '$ENVELOPE)
        for body = (getf msg '$BODY[])
        for filename = (maildir-message-filename store uid)
        for tmpname = (fad:merge-pathnames-as-file (store-folder store "tmp/") filename)
        when (and uid body)
          do (dbi:with-transaction db
               (unless (store-get-by-uid store uid)
                 (v:debug :store "Saving message ~D ~A ~A" uid flags filename)
                 (with-open-file (out tmpname :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                   (write-string (if labels
                                     (%add-x-keywords body str-labels)
                                     body)
                                 out))
                 (let ((newname (fad:merge-pathnames-as-file
                                 (if seen
                                     (store-folder store "cur/")
                                     (store-folder store "new/"))
                                 (if seen
                                     (with-output-to-string (out)
                                       (format out "~A:2," filename)
                                       (when (find '$\\Draft flags) (write-char #\D out))
                                       (when (find '$\\Flagged flags) (write-char #\F out))
                                       (when (find '$\\Answered flags) (write-char #\R out))
                                       (when seen (write-char #\S out))
                                       (when (find '$\\Deleted flags) (write-char #\T out)))
                                     filename))))
                   (rename-file tmpname newname)
                   (setf mtime (file-attributes:modification-time newname)))
                 (store-save-flags store str-flags) ; XXX: probably pointless..
                 (store-save-labels store str-labels)
                 (dbi:execute insert-message (list uid uid filename internaldate mtime))
                 (loop for flag in str-flags
                       do (dbi:execute add-flags (list flag uid)))
                 (loop for label in str-labels
                       do (dbi:execute add-labels (list label uid)))))))
