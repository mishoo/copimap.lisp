;;;; package.lisp

(defpackage #:copimap
  (:use #:cl #:named-readtables)
  (:local-nicknames (#:sock #:usocket)
                    (#:rx #:cl-ppcre)
                    (#:v #:org.shirakumo.verbose)
                    (#:file-attributes #:org.shirakumo.file-attributes)
                    (#:a #:alexandria)))

(defpackage #:copimap-atoms)

(in-package #:copimap)

(defparameter *preserve-case-readtable* (copy-readtable *readtable*))
(setf (readtable-case *preserve-case-readtable*) :preserve)

(defreadtable copimap::syntax
  (:merge :standard))
