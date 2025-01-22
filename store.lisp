(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass store ()
  ((path :initarg :path :accessor store-path)
   (db :accessor store-db)
   (q-insert-message :accessor q-insert-message)
   (q-add-flags :accessor q-add-flags)
   (q-del-flags :accessor q-del-flags)
   (q-add-labels :accessor q-add-labels)
   (q-del-labels :accessor q-del-labels)))

(defun store-db-filename (store)
  (uiop:native-namestring
   (fad:merge-pathnames-as-file (store-path store) ".imapsync.sqlite3")))

(defun store-folder (store &optional folder)
  (uiop:native-namestring
   (if folder
       (fad:merge-pathnames-as-directory (store-path store) folder)
       (store-path store))))

(defgeneric store-get-last-uid (store))
(defgeneric store-save-mailbox (store mailbox))
(defgeneric store-save-messages (store imap messages))
(defgeneric store-find-local-changes (store))
(defgeneric store-get-by-uid (store uid))
(defgeneric store-get-by-path (store path))

(defmethod initialize-instance :after ((store store) &key &allow-other-keys)
  (with-slots (path db) store
    (setf path (fad:pathname-as-directory path))
    (ensure-directories-exist path)
    (let ((db-file (store-db-filename store)))
      (unless (fad:file-exists-p db-file)
        (with-open-file (out db-file :if-does-not-exist :create)))
      (setf db (dbi:connect :sqlite3 :database-name db-file))
      (dbi:do-sql db "PRAGMA foreign_keys = ON")
      (store-upgrade-db store nil)
      (setf (q-insert-message store)
            (dbi:prepare db "INSERT INTO message (uid, local_uid, path, internaldate, message_id, mtime) VALUES (?, ?, ?, ?, ?, ?)"))
      (setf (q-add-flags store)
            (dbi:prepare db "INSERT INTO map_flag_message (flag, message) VALUES (?, ?)"))
      (setf (q-del-flags store)
            (dbi:prepare db "DELETE FROM map_flag_message WHERE message = ?"))
      (setf (q-add-labels store)
            (dbi:prepare db "INSERT INTO map_label_message (label, message) VALUES (?, ?)"))
      (setf (q-del-labels store)
            (dbi:prepare db "DELETE FROM map_label_message WHERE message = ?")))))

(defmethod store-save-mailbox ((store store) (mailbox mailbox))
  (loop with db = (store-db store)
        with query = (dbi:prepare db "INSERT INTO mailbox (key, intval) VALUES (?, ?)
                                      ON CONFLICT(key) DO UPDATE SET intval = ?")
        for key in '(exists recent unseen uidvalidity uidnext highestmodseq)
        for val = (slot-value mailbox key)
        do (dbi:execute query (list (symbol-name key) val))))

(defmethod store-insert-message ((store store)
                                 &key
                                   uid
                                   (local-uid uid)
                                   path
                                   internaldate
                                   message-id
                                   mtime
                                   flags
                                   labels)
  (dbi:execute (q-insert-message store)
               (list uid local-uid path internaldate message-id mtime))
  (store-set-flags store uid flags)
  (store-set-labels store uid labels))

(defmethod store-set-flags ((store store) uid flags)
  (dbi:execute (q-del-flags store) (list uid))
  (loop for flag in flags
        do (dbi:execute (q-add-flags store) (list flag uid))))

(defmethod store-set-labels ((store store) uid labels)
  (dbi:execute (q-del-labels store) (list uid))
  (loop for label in labels
        do (dbi:execute (q-add-labels store) (list label uid))))

(defmethod store-get-last-uid ((store store))
  (sql-single (store-db store)
              "SELECT uid FROM message ORDER BY uid DESC LIMIT 1"))

(defmethod store-get-by-uid ((store store) uid)
  (sql-row (store-db store) "SELECT * FROM message WHERE uid = ?" uid))

(defmethod store-get-by-path ((store store) path)
  (sql-row (store-db store) "SELECT * FROM message WHERE path = ?" path))

;;;;;;;;;;;;;;;;

(defclass maildir-store (store)
  ())

(defmethod initialize-instance :after ((store maildir-store) &key &allow-other-keys)
  (ensure-directories-exist (store-folder store "new/"))
  (ensure-directories-exist (store-folder store "cur/"))
  (ensure-directories-exist (store-folder store "tmp/")))

(defgeneric maildir-make-filename (store uid envelope))
(defgeneric maildir-find-message-file (store uid))

(let ((prev-time 0) (id 0))
  (defun make-maildir-filename (uid)
    (let* ((time (get-universal-time))
           (prefix (if (= time prev-time)
                       (format nil "~D_~D" time (incf id))
                       (format nil "~D" (setf id 0 prev-time time)))))
      (format nil "imapsync.U=~D.~A.~A"
              uid
              prefix
              (machine-instance)))))

(defun envelope-message-id (envelope)
  (destructuring-bind (date subject from sender reply-to to cc bcc in-reply-to message-id)
      envelope
    (declare (ignore date subject from sender reply-to to cc bcc in-reply-to))
    message-id))

(defmethod maildir-make-filename ((store maildir-store) uid envelope)
  (make-maildir-filename uid))

(defmethod maildir-find-message-file ((store maildir-store) uid)
  "Return the current filename for the given `store' and `uid' (it could
have moved from new/ to cur/, or it could have various flags
appended)."
  ;; XXX: this is ugly (runs external "find" program), but much faster
  ;; than using `directory', since maildirs could have tens of
  ;; thousands of files. Running `directory' on my Gmail folder takes
  ;; 2 seconds and conses a list of 40k names (there seems to be no
  ;; standard way to use a wildcard for part of the filename).
  ;;
  ;; Should check `iolib/os:walk-directory'.
  (let ((msg (store-get-by-uid store uid)))
    (when msg
      (let ((filename
              (rx:regex-replace
               "\\s+$"
               (with-output-to-string (out)
                 (uiop:run-program
                  `("find" ,(store-folder store)
                           "-type" "f"
                           "-name" ,(format nil "~A*" (getf msg :|path|)))
                  :output out))
               "")))
        (if (string= filename "") nil filename)))))

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
        with insert-message = (q-insert-message store)
        with add-flags = (q-add-flags store)
        with add-labels = (q-add-labels store)
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
        for message-id = (when envelope (envelope-message-id envelope))
        for body = (getf msg '$BODY[])
        for filename = (maildir-make-filename store uid envelope)
        for tmpname = (fad:merge-pathnames-as-file (store-folder store "tmp/") filename)
        do (dbi:with-transaction db
             (cond
               ((store-get-by-uid store uid)
                (store-set-flags store uid str-flags)
                (store-set-labels store uid str-labels))
               (body
                (v:debug :store "Saving message ~D ~A ~A" uid flags filename)
                (with-open-file (out tmpname :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create
                                             :external-format :iso-8859-1)
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
                                      (when (find '$$Forwarded flags) (write-char #\P out))
                                      (when (find '$\\Answered flags) (write-char #\R out))
                                      (when seen (write-char #\S out))
                                      (when (find '$\\Deleted flags) (write-char #\T out)))
                                    filename))))
                  (rename-file tmpname newname)
                  (setf mtime (file-attributes:modification-time newname)))
                (store-insert-message store :uid uid
                                            :path filename
                                            :internaldate internaldate
                                            :message-id message-id
                                            :mtime mtime
                                            :flags str-flags
                                            :labels str-labels))))))

(defun maildir-flags-from-file (strflags)
  (loop for ch across strflags
        for f = (assoc ch '((#\S . $\\Seen)
                            (#\R . $\\Answered)
                            (#\F . $\\Flagged)
                            (#\T . $\\Deleted)
                            (#\D . $\\Draft)
                            (#\P . $$Forwarded))
                       :test #'eql)
        when f collect (cdr f)))

(defmacro for-maildir-files ((dir &optional
                                    (filename 'filename)
                                    (fullname 'fullname)
                                    (mtime 'mtime)
                                    (path 'path)
                                    (flags 'flags))
                             &body body)
  (a:once-only (dir)
    (a:with-gensyms (process in)
      `(let* ((,process (uiop:launch-program `("ls" "--sort" "t" ,,dir)
                                             :output :stream))
              (,in (uiop:process-info-output ,process)))
         (loop with ,flags
               for ,filename = (read-line ,in nil)
               while ,filename
               for ,fullname = (fad:merge-pathnames-as-file ,dir ,filename)
               for ,mtime = (file-attributes:modification-time ,fullname)
               for ,path = (rx:register-groups-bind (mpath mflags)
                               ("^(.*?)(?::2,([^:]*))?$" ,filename)
                             (when mflags
                               (setf ,flags (maildir-flags-from-file mflags)))
                             mpath)
               ,@body
               finally (uiop:close-streams ,process)
                       (uiop:terminate-process ,process))))))

(defun maildir-find-changed-files (store dir)
  (setf dir (store-folder store dir))
  (for-maildir-files (dir filename fullname mtime path flags)
    for msg = (when path (store-get-by-path store path))
    until (and msg (eql mtime (getf msg :|mtime|)))
    collect (list msg fullname flags mtime)))

(defmethod store-find-local-changes ((store maildir-store))
  (loop for (msg fullname flags mtime) in `(,@(maildir-find-changed-files store "new/")
                                            ,@(maildir-find-changed-files store "cur/"))
        do (format t "~A ~A~%" mtime msg)))

(defun maildir-import-offlineimap (path)
  (setf path (fad:pathname-as-directory path))
  (let* ((store (make-instance 'maildir-store :path path))
         (db (store-db store)))
    (labels ((add-dir (dir)
               (for-maildir-files ((store-folder store dir))
                 for uid = (rx:register-groups-bind (uid) ("U=(\\d+)" filename)
                             (declare (type string uid))
                             (parse-integer uid))
                 when uid do
                 (let* ((head (with-open-file (in fullname
                                                  :direction :input
                                                  :external-format :iso-8859-1)
                                (parse-rfc822-headers in)))
                        (message-id (get-header head "Message-Id"))
                        (date (get-header head "Date"))
                        (labels (get-header head "X-Keywords")))
                   (when labels (setf labels (decode-header labels)))
                   (dbi:with-transaction db
                     (cond
                       ((store-get-by-uid store uid)
                        (return))
                       (t
                        (v:debug :import "UID: ~A, Flags: ~A, Labels: ~A" uid flags labels)
                        (store-insert-message
                         store
                         :uid uid
                         :path path
                         :internaldate (when date
                                         (ignore-errors (cl-date-time-parser:parse-date-time date)))
                         :message-id message-id
                         :mtime mtime
                         :flags (mapcar #'as-string flags)
                         :labels (when labels
                                   (rx:split "\\s*,\\s*" labels))))))))))
      (add-dir "new/")
      (add-dir "cur/")
      store)))
