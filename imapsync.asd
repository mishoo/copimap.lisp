;;;; imapsync.asd

(asdf:defsystem #:imapsync
  :description "IMAP client/sync library"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:usocket
               #:cl+ssl
               #:usocket
               #:flexi-streams
               #:named-readtables
               #:trivial-utf-8
               #:cl-ppcre
               #:bordeaux-threads
               #:babel
               #:verbose)
  :components ((:file "package")
               (:file "parse")
               (:file "mUTF-7")
               (:file "imap")))
