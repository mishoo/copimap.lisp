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

(defun sqlite-table-exists (db table)
  (dbi:fetch
   (dbi:execute
    (dbi:prepare db
        "SELECT name FROM sqlite_master WHERE type = 'table' and name = ? LIMIT 1")
    (list table))))

(defgeneric store-get-last-uid (store))
(defgeneric store-save-messages (store messages))
(defgeneric store-save-flags (store flags))
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
      (unless (sqlite-table-exists db "message")
        (store-create-db store)))))

(defgeneric store-create-db (store)
  (:method ((store store))
    (let ((queries
            '("CREATE TABLE message (
                 uid UNSIGNED INTEGER PRIMARY KEY,
                 path VARCHAR(255),
                 internaldate INTEGER,
                 mtime UNSIGNED INTEGER
               )"
              "CREATE INDEX idx_message_mtime ON message(mtime)"
              "CREATE TABLE flag (name VARCHAR(255) PRIMARY KEY)"
              "CREATE TABLE map_flag_message (
                 flag VARCHAR(255),
                 message UNSIGNED INTEGER,
                 PRIMARY KEY (flag, message),
                 FOREIGN KEY (flag) REFERENCES flags(name) ON DELETE CASCADE ON UPDATE CASCADE,
                 FOREIGN KEY (message) REFERENCES messages(uid) ON DELETE CASCADE ON UPDATE CASCADE
               )"
              )))
      (loop with db = (store-db store)
            for q in queries
            do (cl-dbi:do-sql db q)))))

(defmethod store-save-flags ((store store) flags)
  (let ((sql (dbi:prepare (store-db store)
                 "INSERT INTO flag (name) VALUES (?) ON CONFLICT DO NOTHING")))
    (loop for name in flags do
      (dbi:execute sql (list (if (symbolp name)
                                 (symbol-name name)
                                 name))))))

(defmethod store-get-last-uid ((store store))
  (car (dbi:fetch
        (dbi:execute
         (dbi:prepare (store-db store)
             "SELECT uid FROM message ORDER BY uid DESC LIMIT 1")))))

(defmethod store-get-by-uid ((store store) uid)
  (car (dbi:fetch
        (dbi:execute
         (dbi:prepare (store-db store)
             "SELECT * FROM message WHERE uid = ?")
         (list uid)))))

(defmethod store-close ((store store))
  (dbi:disconnect (store-db store)))

;;;;;;;;;;;;;;;;

(defclass maildir-store (store)
  ())

(defmethod initialize-instance :after ((store maildir-store) &key &allow-other-keys)
  (with-slots (path db) store
    (ensure-directories-exist (store-folder store "new/"))
    (ensure-directories-exist (store-folder store "cur/"))
    (ensure-directories-exist (store-folder store "tmp/"))))

(defgeneric maildir-message-filename (store uid))

(let ((prev-time 0) (id 0))
  (defmethod maildir-message-filename ((store maildir-store) uid)
    (let* ((time (get-universal-time))
           (prefix (if (= time prev-time)
                       (format nil "~D_~D" time (incf id))
                       (progn
                         (setf prev-time time
                               id 0)
                         (format nil "~D" time)))))
      (format nil "~A.~A,U=~D"
              prefix
              (machine-instance)
              uid))))

(defmethod store-save-messages ((store maildir-store) messages)
  (loop with db = (store-db store)
        with insert-message = (dbi:prepare db "INSERT INTO message (uid, path, internaldate, mtime) VALUES (?, ?, ?, ?)")
        with add-flags = (dbi:prepare db "INSERT INTO map_flag_message (flag, message) VALUES (?, ?)")
        with mtime
        for msg in messages
        for uid = (getf msg '$UID)
        for internaldate = (let* ((date (getf msg '$INTERNALDATE))
                                  (ts (when date (parse-internaldate date))))
                             (when ts (local-time:timestamp-to-unix ts)))
        for flags = (getf msg '$FLAGS)
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
                   (write-string body out))
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
                 (store-save-flags store flags) ; XXX: probably pointless..
                 (dbi:execute insert-message (list uid filename internaldate mtime))
                 (loop for flag in flags
                       for flag-name = (if (symbolp flag)
                                           (symbol-name flag)
                                           flag)
                       do (dbi:execute add-flags (list flag-name uid)))))))
