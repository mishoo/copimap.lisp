(in-package #:copimap)

(in-readtable copimap::syntax)

(defclass store ()
  ((path :initarg :path :accessor store-path)
   (db :accessor store-db)
   (q-insert-message :accessor q-insert-message)
   (q-del-message :accessor q-del-message)
   (q-add-flags :accessor q-add-flags)
   (q-del-flags :accessor q-del-flags)
   (q-get-flags :accessor q-get-flags)
   (q-add-labels :accessor q-add-labels)
   (q-del-labels :accessor q-del-labels)
   (q-get-labels :accessor q-get-labels)
   (q-set-encountered :accessor q-set-encountered)
   (q-get-metadata :accessor q-get-metadata)
   (q-set-metadata :accessor q-set-metadata)))

(defun store-db-filename (store)
  (uiop:native-namestring
   (fad:merge-pathnames-as-file (store-path store) ".copimap.sqlite3")))

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
(defgeneric store-get-metadata (store key))
(defgeneric store-set-metadata (store key &key intval txtval binval))

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
      (setf (q-del-message store)
            (dbi:prepare db "DELETE FROM message WHERE uid = ?"))
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
            (dbi:prepare db "SELECT label FROM map_label_message WHERE message = ?"))
      (setf (q-set-encountered store)
            (dbi:prepare db "UPDATE message SET encountered = 1, path = ? WHERE uid = ?"))
      (setf (q-get-metadata store)
            (dbi:prepare db "SELECT intval, txtval, binval FROM mailbox WHERE key = ?"))
      (setf (q-set-metadata store)
            (dbi:prepare db "INSERT INTO mailbox (key, intval, txtval, binval) VALUES (?, ?, ?, ?)
                             ON CONFLICT(key) DO UPDATE SET intval = ?, txtval = ?, binval = ?")))))

(defmethod store-save-mailbox ((store store) (mailbox mailbox))
  (loop with db = (store-db store)
        with query = (dbi:prepare db "INSERT INTO mailbox (key, intval) VALUES (?, ?)
                                      ON CONFLICT(key) DO UPDATE SET intval = ?")
        for key in '(exists recent unseen uidvalidity uidnext)
        for val = (slot-value mailbox key)
        do (dbi:execute query (list (symbol-name key) val val))))

(defmethod store-get-metadata ((store store) (key string))
  (dbi:fetch (dbi:execute
              (q-get-metadata store) (list key))
             :format :values))

(defmethod store-get-metadata ((store store) (key symbol))
  (store-get-metadata store (symbol-name key)))

(defmethod store-set-metadata ((store store) (key string) &key intval txtval binval)
  (dbi:execute
   (q-set-metadata store) (list key intval txtval binval intval txtval binval)))

(defmethod store-set-metadata ((store store) (key symbol) &rest args &key &allow-other-keys)
  (apply #'store-set-metadata store (symbol-name key) args))

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
  (sql-row (store-db store) "SELECT * FROM message WHERE path like ? || '%'" path))

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
      (format nil "copimap.U=~D.~A.~A"
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
                 (with-output-to-string (err)
                   (handler-case
                       (uiop:run-program
                        `("find" "-L" "new/" "cur/"
                                 "-type" "f"
                                 "-name" ,(format nil "~A*"
                                                  (rx:register-groups-bind (name)
                                                      ("^.*?/(.*?)(?::2,([^:]*))?$"
                                                       (getf msg :|path|))
                                                    name)))
                        :output out
                        :error-output err
                        :directory (store-folder store))
                     (error (x)
                       (v:error :ERROR "~A" x)
                       (v:error :ERROR "~A" (get-output-stream-string err))))))
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
        with filename
        with tmpname
        for msg in messages
        for uid = (getf msg '$UID)
        for modseq = (car (getf msg '$MODSEQ))
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
        do (dbi:with-transaction db
             (cond
               ((store-get-by-uid store uid)
                (v:debug :store "Updating message ~D ~A ~A" uid flags labels)
                (store-set-flags store uid str-flags)
                (store-set-labels store uid str-labels))
               (body
                (setf filename (maildir-make-filename store uid envelope)
                      tmpname (fad:merge-pathnames-as-file (store-folder store "tmp/") filename))
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
                                            :labels str-labels)))
             (when modseq
               (let ((highestmodseq (mailbox-highestmodseq conn)))
                 (when (or (not highestmodseq)
                           (< highestmodseq modseq))
                   (store-set-metadata store '$HIGHESTMODSEQ :intval modseq)
                   (setf (mailbox-highestmodseq conn) modseq)))))))

(defun maildir-flags-from-file-suffix (strflags)
  (loop for ch across strflags
        for fl = (assoc ch '((#\D . "\\Draft")
                             (#\F . "\\Flagged")
                             (#\P . "$Forwarded")
                             (#\R . "\\Answered")
                             (#\S . "\\Seen")
                             (#\T . "\\Deleted"))
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

(defmacro for-maildir-files ((dir &key
                                    (filename 'filename)
                                    (fullname 'fullname)
                                    (mtime 'mtime)
                                    (flags 'flags)
                                    (uid 'uid))
                             &body body)
  (a:once-only (dir)
    (a:with-gensyms (process in line)
      `(let* ((,process (uiop:launch-program `("find" "-L" "new/" "cur/"
                                                      "-type" "f"
                                                      "-printf" "%Ts %p\\n")
                                             :directory ,dir
                                             :output :stream))
              (,in (uiop:process-info-output ,process)))
         (loop with ,flags and ,filename and ,fullname and ,mtime
               for ,line = (read-line ,in nil)
               while ,line
               for ,uid = (rx:register-groups-bind (_mtime _filename _uid _flags)
                              ("^(\\d+) (.*?U=(\\d+).*?(?::2,([^:]*))?)$" ,line)
                            (declare (type string _mtime _filename _uid)
                                     (type (or null string) _flags))
                            (setf ,mtime (+ (the fixnum (parse-integer _mtime))
                                            #.(encode-universal-time 0 0 0 1 1 1970 0))
                                  ,filename _filename
                                  ,fullname (concatenate 'string ,dir _filename))
                            (when _flags
                              (setf ,flags (maildir-flags-from-file-suffix _flags)))
                            (parse-integer _uid))
               ,@body
               finally (uiop:close-streams ,process)
                       (uiop:terminate-process ,process))))))

(defmethod store-find-local-changes ((store maildir-store))
  (labels ((get-x-keywords (file)
             (let (kw)
               (ignore-errors
                (with-open-file (input file :direction :input
                                            :external-format :iso-8859-1)
                  (parse-rfc822-headers
                   input
                   (lambda (x)
                     (when (string-equal "X-Keywords" (car x))
                       (setf kw (cdr x))
                       t)))))
               (when kw (rx:split "\\s*,\\s*" (decode-header kw))))))
    (let ((changes nil)
          (db (store-db store)))
      (dbi:with-transaction db
        (dbi:do-sql db "UPDATE message SET encountered = 0")
        (for-maildir-files ((store-folder store))
          for msg = (when uid (store-get-by-uid store uid))
          for stuff = nil
          when msg do
          (dbi:execute (q-set-encountered store) (list filename uid))
          (let ((saved-mtime (getf msg :|mtime|)))
            (declare (type fixnum saved-mtime))
            (unless (= mtime saved-mtime)
              (let* ((saved-labels (store-get-labels store uid))
                     (new-labels (get-x-keywords fullname))
                     (added (set-difference new-labels saved-labels :test #'equal))
                     (removed (set-difference saved-labels new-labels :test #'equal)))
                (when (or added removed)
                  (setf stuff `(,@stuff :+label ,added :-label ,removed
                                        :set-mtime ,mtime :set-labels ,new-labels))))))
          (let ((saved-path (getf msg :|path|)))
            (unless (string-equal filename saved-path)
              (let* ((saved-flags (store-get-flags store uid))
                     (new-flags flags)
                     (added (set-difference new-flags saved-flags :test #'equal))
                     (removed (set-difference saved-flags new-flags :test #'equal)))
                (when (or added removed)
                  (setf stuff `(,@stuff :+flags ,added :-flags ,removed
                                        :set-path ,filename :set-flags ,new-flags))))))
          (when stuff
            (push (cons uid stuff) changes)))
        (let ((removed
                (mapcar #'cadr (dbi:fetch-all
                                (dbi:execute
                                 (dbi:prepare db "SELECT uid FROM message WHERE NOT encountered"))))))
          (when removed
            (push (cons 'expunged removed) changes))))
      changes)))

(defmethod store-update-for-changes ((store maildir-store) changes)
  (let ((db (store-db store)))
    (dbi:with-transaction db
      (loop for (uid . data) in changes
            do (if (eq uid 'expunged)
                   (loop for uid in data do
                     (dbi:execute (q-del-message store) (list uid)))
                   (destructuring-bind (&key
                                          set-labels
                                          set-flags
                                          set-mtime
                                          set-path
                                        &allow-other-keys)
                       data
                     (when set-labels
                       (store-set-labels store uid set-labels))
                     (when set-flags
                       (store-set-flags store uid set-flags))
                     (when set-mtime
                       (dbi:execute
                        (dbi:prepare db "UPDATE message SET mtime = ? WHERE uid = ?")
                        (list set-mtime uid)))
                     (when set-path
                       (dbi:execute
                        (dbi:prepare db "UPDATE message SET path = ? WHERE uid = ?")
                        (list set-path uid)))))))))

(defun maildir-import-offlineimap (path)
  (declare (optimize (speed 3)))
  (setf path (fad:pathname-as-directory path))
  (let* ((store (make-instance 'maildir-store :path path))
         (db (store-db store))
         (count 0))
    (declare (type fixnum count))
    (for-maildir-files ((store-folder store))
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
        (setf keywords (when keywords
                         (rx:split "\\s*,\\s*" keywords)))
        (dbi:with-transaction db
          (cond
            ((store-get-by-uid store uid)
             (store-set-flags store uid flags)
             (store-set-labels store uid keywords))
            (t
             ;; (v:debug :import "UID: ~A, Flags: ~A, Labels: ~A" uid flags keywords)
             (store-insert-message
              store
              :uid uid
              :path filename
              :internaldate (when date
                              (ignore-errors (cl-date-time-parser:parse-date-time date)))
              :message-id message-id
              :mtime mtime
              :flags flags
              :labels keywords))))))
    (v:debug :import "checked in ~D messages" count)
    store))
