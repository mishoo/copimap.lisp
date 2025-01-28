(in-package #:imapsync)

(in-readtable imapsync::syntax)

(defclass store ()
  ((path :initarg :path :accessor store-path)
   (db :accessor store-db)
   (q-insert-message :accessor q-insert-message)
   (q-add-flags :accessor q-add-flags)
   (q-del-flags :accessor q-del-flags)
   (q-get-flags :accessor q-get-flags)
   (q-add-labels :accessor q-add-labels)
   (q-del-labels :accessor q-del-labels)
   (q-get-labels :accessor q-get-labels)))

(defun store-db-filename (store)
  (uiop:native-namestring
   (fad:merge-pathnames-as-file (store-path store) ".imapsync.sqlite3")))

(defun store-folder (store &optional folder)
  (uiop:native-namestring
   (if folder
       (fad:merge-pathnames-as-directory (store-path store) folder)
       (store-path store))))

(defun store-file (store file)
  (uiop:native-namestring
   (fad:merge-pathnames-as-file (store-path store) file)))

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
      (setf (q-get-flags store)
            (dbi:prepare db "SELECT flag FROM map_flag_message WHERE message = ?"))
      (setf (q-add-labels store)
            (dbi:prepare db "INSERT INTO map_label_message (label, message) VALUES (?, ?)"))
      (setf (q-del-labels store)
            (dbi:prepare db "DELETE FROM map_label_message WHERE message = ?"))
      (setf (q-get-labels store)
            (dbi:prepare db "SELECT label FROM map_label_message WHERE message = ?")))))

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

(defmethod store-get-flags ((store store) uid)
  (mapcar #'second
          (dbi:fetch-all
           (dbi:execute (q-get-flags store) (list uid)))))

(defmethod store-set-labels ((store store) uid labels)
  (dbi:execute (q-del-labels store) (list uid))
  (loop for label in labels
        do (dbi:execute (q-add-labels store) (list label uid))))

(defmethod store-get-labels ((store store) uid)
  (mapcar #'second
          (dbi:fetch-all
           (dbi:execute (q-get-labels store) (list uid)))))

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

(defun maildir-find-message-file (store uid)
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
                           "-name" ,(format nil "~A*"
                                            (rx:register-groups-bind (name)
                                                ("^.*?/(.*?)(?::2,([^:]*))?$"
                                                 (getf msg :|path|))
                                              name)))
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
                (let* ((newname
                         (concatenate 'string
                                      (if seen "cur/" "new/")
                                      (let ((suffix (maildir-file-suffix-from-flags flags)))
                                        (if (plusp (length suffix))
                                            (format nil "~A:2,~A" filename suffix)
                                            filename))))
                       (newname-full (store-file store newname)))
                  (rename-file tmpname newname-full)
                  (setf mtime (file-attributes:modification-time newname-full))
                  (setf filename newname))
                (store-insert-message store :uid uid
                                            :path filename
                                            :internaldate internaldate
                                            :message-id message-id
                                            :mtime mtime
                                            :flags str-flags
                                            :labels str-labels))))))

(defun maildir-flags-from-file-suffix (strflags)
  (loop for ch across strflags
        for fl = (assoc ch '((#\D . $\\Draft)
                             (#\F . $\\Flagged)
                             (#\P . $$Forwarded)
                             (#\R . $\\Answered)
                             (#\S . $\\Seen)
                             (#\T . $\\Deleted))
                        :test #'eql)
        when fl collect (cdr fl)))

(defun maildir-file-suffix-from-flags (flags)
  (with-output-to-string (out)
    (loop for fl in flags do
      (case fl
        ($\\Draft (write-char #\D out))
        ($\\Flagged (write-char #\F out))
        ($$Forwarded (write-char #\P out))
        ($\\Answered (write-char #\R out))
        ($\\Seen (write-char #\S out))
        ($\\Deleted (write-char #\T out))))))

(defmacro for-maildir-files ((dir &optional
                                    (filename 'filename)
                                    (fullname 'fullname)
                                    (mtime 'mtime)
                                    (fileroot 'fileroot)
                                    (flags 'flags))
                             &body body)
  (a:once-only (dir)
    (a:with-gensyms (process in)
      `(let* ((,process (uiop:launch-program `("ls" "--literal" "--sort" "t" ,,dir)
                                             :output :stream))
              (,in (uiop:process-info-output ,process)))
         (loop with ,flags
               for ,filename = (read-line ,in nil)
               while ,filename
               for ,fullname = (fad:merge-pathnames-as-file ,dir ,filename)
               for ,mtime = (file-attributes:modification-time ,fullname)
               for ,fileroot = (rx:register-groups-bind (mfileroot mflags)
                                   ("^(.*?)(?::2,([^:]*))?$" ,filename)
                                 (when mflags
                                   (setf ,flags (maildir-flags-from-file-suffix mflags)))
                                 mfileroot)
               ,@body
               finally (uiop:close-streams ,process)
                       (uiop:terminate-process ,process))))))

(defun maildir-store-get-files (store dir &optional (changed-only t))
  (for-maildir-files ((store-folder store dir))
    for path = (concatenate 'string dir filename)
    for msg = (store-get-by-path store path)
    until (and changed-only msg (eql mtime (getf msg :|mtime|)))
    when msg collect (list msg fullname flags mtime)))

(defmethod store-find-local-changes ((store maildir-store))
  (declare (optimize (speed 3)))
  (labels ((get-x-keywords (file)
             (let (kw)
               (ignore-errors
                (with-open-file (input file :direction :input
                                            :external-format :iso-8859-1)
                  (parse-rfc822-headers
                   input
                   (lambda (x)
                     (when (string-equal "X-Keywords" (car x))
                       (setf kw (cdr x)))))))
               (when kw (rx:split "\\s*,\\s*" (decode-header kw))))))
    (let (changed-labels changed-flags)
      (setf changed-labels
            (loop for (msg fullname flags mtime)
                    in `(,@(maildir-store-get-files store "new/" t)
                         ,@(maildir-store-get-files store "cur/" t))
                  for uid = (getf msg :|uid|)
                  for labels = (store-get-labels store uid)
                  for new-labels = (get-x-keywords fullname)
                  for added = (set-difference new-labels labels :test #'equal)
                  for removed = (set-difference labels new-labels :test #'equal)
                  collect `(,uid ,added ,removed)))
      `(:changed-labels ,changed-labels))))

(defun maildir-import-offlineimap (path)
  (declare (optimize (speed 3)))
  (setf path (fad:pathname-as-directory path))
  (let* ((store (make-instance 'maildir-store :path path))
         (db (store-db store)))
    (labels ((add-dir (dir &aux (count 0))
               (declare (type fixnum count))
               (for-maildir-files ((store-folder store dir))
                 for uid = (rx:register-groups-bind (uid) ("U=(\\d+)" filename)
                             (declare (type string uid))
                             (parse-integer uid))
                 when uid do
                 (incf count)
                 (let* (message-id date keywords)
                   (with-open-file (input fullname
                                          :direction :input
                                          :external-format :iso-8859-1)
                     (parse-rfc822-headers
                      input
                      (lambda (x)
                        (declare (type cons x))
                        (destructuring-bind (header . value) x
                          (ignore-errors
                           (cond
                             ((string-equal header "Message-Id")
                              (setf message-id (decode-header value t)))
                             ((string-equal header "Date")
                              (setf date (decode-header value t)))
                             ((string-equal header "X-Keywords")
                              (setf keywords (decode-header value)))))
                          nil))))
                   (setf flags (mapcar #'as-string flags)
                         keywords (when keywords
                                    (rx:split "\\s*,\\s*" keywords)))
                   (dbi:with-transaction db
                     (cond
                       ((store-get-by-uid store uid)
                        (store-set-flags store uid flags)
                        (store-set-labels store uid keywords))
                       (t
                        (v:debug :import "UID: ~A, Flags: ~A, Labels: ~A" uid flags keywords)
                        (store-insert-message
                         store
                         :uid uid
                         :path (concatenate 'string dir filename)
                         :internaldate (when date
                                         (ignore-errors (cl-date-time-parser:parse-date-time date)))
                         :message-id message-id
                         :mtime mtime
                         :flags flags
                         :labels keywords))))))
               (v:debug :import "~A: checked in ~D messages" dir count)))
      (add-dir "new/")
      (add-dir "cur/")
      store)))
