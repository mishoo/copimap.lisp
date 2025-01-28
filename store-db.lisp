(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defun sql-row (db query &rest args)
  (dbi:fetch
   (dbi:execute
    (dbi:prepare db query)
    args)))

(defun sql-single (db query &rest args)
  (cadr (apply #'sql-row db query args)))

(defun sql-table-exists (db table)
  (sql-row db "SELECT name FROM sqlite_master
               WHERE type = 'table' and name = ? LIMIT 1" table))

(defun sql-do-all (db &rest queries)
  (loop for q in queries do (cl-dbi:do-sql db q))
  nil)

(defgeneric store-upgrade-db (store version))

(defmethod store-upgrade-db ((store store) (version (eql nil)))
  (let ((db (store-db store)))
    (setf version
          (if (sql-table-exists db "mailbox")
              (sql-single db "SELECT intval FROM mailbox WHERE key = '_db_version_' LIMIT 1")
              0))
    (loop until (store-upgrade-db store version)
          do (incf version))
    (dbi:do-sql db
      "INSERT INTO mailbox (key, intval) VALUES ('_db_version_', ?)
       ON CONFLICT(key) DO UPDATE SET intval = ?"
      (list version version))))

(defmethod store-upgrade-db ((store store) version)
  (v:debug :store "Store DB up-to-date (version ~A)" version)
  t)

(defmethod store-upgrade-db ((store store) (version (eql 0)))
  (v:debug :store "New database ~A" (store-db-filename store))
  (sql-do-all
   (store-db store)
   "CREATE TABLE mailbox (
      key VARCHAR(255) NOT NULL PRIMARY KEY,
      intval INTEGER,
      txtval TEXT,
      binval BLOB)"

   "CREATE TABLE message (
      uid UNSIGNED INTEGER PRIMARY KEY,
      local_uid UNSIGNED INTEGER UNIQUE,
      path VARCHAR(255) UNIQUE,
      internaldate INTEGER,
      message_id VARCHAR(255),
      mtime UNSIGNED INTEGER,
      encountered BOOLEAN NOT NULL DEFAULT 0)"

   "CREATE INDEX idx_message_mtime ON message(mtime)"

   "CREATE INDEX idx_message_message_id ON message(message_id)"

   "CREATE INDEX idx_message_encountered ON message(encountered)"

   "CREATE TABLE map_flag_message (
      flag VARCHAR(255),
      message UNSIGNED INTEGER,
      PRIMARY KEY (flag, message),
      FOREIGN KEY (message) REFERENCES message(uid) ON DELETE CASCADE ON UPDATE CASCADE)"

   "CREATE INDEX idx_map_flag_message_flag ON map_flag_message(flag)"

   "CREATE TABLE map_label_message (
      label VARCHAR(255),
      message UNSIGNED INTEGER,
      PRIMARY KEY (label, message),
      FOREIGN KEY (message) REFERENCES message(uid) ON DELETE CASCADE ON UPDATE CASCADE)"

   "CREATE INDEX idx_map_label_message_label ON map_label_message(label)"))
