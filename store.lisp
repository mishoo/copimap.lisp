(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass store () ())

(defgeneric store-get-last-uid (store))
(defgeneric store-save-messages (store messages))
(defgeneric store-save-flags (store flags))
(defgeneric store-close (store))

;;;;;;;;;;;;;;;;

(defclass maildir-store (store)
  ((path :initarg :path :accessor maildir-path)
   (db :accessor maildir-db)))

(defun maildir-db-filename (store)
  (uiop:native-namestring
   (fad:merge-pathnames-as-file (maildir-path store) ".imapsync.sqlite3")))

(defun maildir-folder (store folder)
  (fad:merge-pathnames-as-directory (maildir-path store) folder))

(defmethod initialize-instance :after ((store maildir-store) &key &allow-other-keys)
  (with-slots (path db) store
    (setf path (fad:pathname-as-directory path))
    (ensure-directories-exist path)
    (ensure-directories-exist (maildir-folder store "new/"))
    (ensure-directories-exist (maildir-folder store "cur/"))
    (ensure-directories-exist (maildir-folder store "tmp/"))
    (let ((db-file (maildir-db-filename store)))
      (unless (fad:file-exists-p db-file)
        (with-open-file (out db-file :if-does-not-exist :create)))
      (setf db (dbi:connect :sqlite3 :database-name db-file))
      (unless (sqlite-table-exists db "message")
        (dbi:do-sql db
          "CREATE TABLE message (
             uid UNSIGNED INTEGER PRIMARY KEY,
             path VARCHAR(255),
             mtime UNSIGNED INTEGER
           )")
        (dbi:do-sql db
          "CREATE TABLE flag (name VARCHAR(255) PRIMARY KEY)")
        (dbi:do-sql db
          "CREATE TABLE map_flag_message (
             message UNSIGNED INTEGER,
             flag VARCHAR(255),
             PRIMARY KEY (message, flag),
             FOREIGN KEY (message) REFERENCES messages(uid) ON DELETE CASCADE ON UPDATE CASCADE,
             FOREIGN KEY (flag) REFERENCES flags(name) ON DELETE CASCADE ON UPDATE CASCADE
           )")))))

(defun sqlite-table-exists (db table)
  (dbi:fetch
   (dbi:execute
    (dbi:prepare db
        "SELECT name FROM sqlite_master WHERE type = 'table' and name = ? LIMIT 1")
    (list table))))

(defmethod store-save-flags ((store maildir-store) flags)
  (let ((sql (dbi:prepare (maildir-db store)
                 "INSERT INTO flag (name) VALUES (?) ON CONFLICT DO NOTHING")))
    (loop for name in flags do
      (dbi:execute sql (list (if (symbolp name)
                                 (symbol-name name)
                                 name))))))

(defmethod store-get-last-uid ((store maildir-store))
  (car (dbi:fetch
        (dbi:execute
         (dbi:prepare (maildir-db store)
             "SELECT uid FROM message ORDER BY uid DESC LIMIT 1")))))

(defmethod store-close ((store maildir-store))
  (dbi:disconnect (maildir-db store)))
