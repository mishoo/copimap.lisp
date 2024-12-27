;;;; package.lisp

(defpackage #:imapsync
  (:use #:cl #:named-readtables)
  (:local-nicknames (:sock :usocket)
                    (:rx :cl-ppcre)))

(defpackage #:imapsync-atoms)

(in-package #:imapsync)

(defreadtable imapsync::syntax
  (:merge :standard))
