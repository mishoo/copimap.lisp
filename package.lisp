;;;; package.lisp

(defpackage #:imapsync
  (:use #:cl #:named-readtables)
  (:local-nicknames (#:sock #:usocket)
                    (#:rx #:cl-ppcre)
                    (#:v #:org.shirakumo.verbose)))

(defpackage #:imapsync-atoms)

(in-package #:imapsync)

(defparameter *preserve-case-readtable* (copy-readtable *readtable*))
(setf (readtable-case *preserve-case-readtable*) :preserve)

(defreadtable imapsync::syntax
  (:merge :standard))
