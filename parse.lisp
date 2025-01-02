(in-package #:imapsync)

(defconstant +atoms-package+ (find-package "IMAPSYNC-ATOMS"))

(in-readtable imapsync::syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character
   #\$ (lambda (stream ch)
         (declare (ignore ch))
         (let ((*package* +atoms-package+)
               (*readtable* (named-readtables:find-readtable :common-lisp)))
           (read stream)))))

(defun %skip-whitespace (input)
  (loop while (char= #\Space (peek-char nil input))
        do (read-char input)))

(defun %read-list (input eol-char utf8)
  (loop until (char= eol-char (peek-char nil input))
        collect (prog2
                    (%skip-whitespace input)
                    (%read-token input utf8)
                  (%skip-whitespace input))
        finally (read-char input)))

(defun %read-token (input utf8)
  (%skip-whitespace input)
  (let ((ch (read-char input)))
    (cond
      ((char= #\Newline ch)
       nil)

      ((char= #\" ch)
       (let ((str (with-output-to-string (out)
                    (loop for ch = (read-char input)
                          until (char= ch #\")
                          do (write-char ch out)))))
         (if utf8 str (mutf-7-to-string str))))

      ((char= #\{ ch)
       (let ((len (%read-token input utf8)))
         (check-type len fixnum)
         (assert (char= #\} (read-char input)))
         (assert (char= #\Newline (read-char input)))
         (if utf8
             (trivial-utf-8:read-utf-8-string (flex:flexi-stream-stream input)
                                              :byte-length len)
             (let ((seq (make-array len :element-type 'character)))
               (read-sequence seq input)
               (mutf-7-to-string seq)))))

      ((char= #\* ch)
       :*)

      ((char= #\( ch)
       (%read-list input #\) utf8))

      ((char= #\[ ch)
       (%read-list input #\] utf8))

      (t
       (cond
         ((char= #\{ (peek-char nil input)) ; literal8
          (read-char input)
          (let ((len (%read-token input utf8)))
            (check-type len fixnum)
            (assert (char= #\} (read-char input)))
            (assert (char= #\Newline (read-char input)))
            (let ((seq (make-array len :element-type '(unsigned-byte 8))))
              (read-sequence seq (flex:flexi-stream-stream input))
              seq)))
         (t
          (let ((atom (with-output-to-string (str)
                        (write-char ch str)
                        (loop with br = 0
                              for ch = (peek-char nil input)
                              until (or (char= #\Newline ch)
                                        (char= #\Space ch)
                                        (find ch "\"*%{}()" :test #'char=)
                                        (and (char= #\] ch)
                                             (zerop br)))
                              do (case ch
                                   (#\[ (incf br))
                                   (#\] (decf br)))
                                 (write-char (read-char input) str)))))
            (rx:register-groups-bind (a b) ("^(\\d+):(\\d+|\\*)$" atom)
              (declare (type string a b))
              (return-from %read-token
                `(:range ,(parse-integer a)
                         ,(if (string= b "*")
                              :*
                              (parse-integer b)))))
            (cond
              ((rx:scan "^\\d+$" atom)
               (parse-integer atom))
              (t
               (unless utf8
                 (setf atom (mutf-7-to-string atom)))
               (intern atom +atoms-package+))))))))))

(defun %maybe-arg (input utf8)
  (%skip-whitespace input)
  (when (char= #\[ (peek-char nil input))
    (prog1
        (%read-token input utf8)
      (%skip-whitespace input))))

(defun %read-cmdline (input utf8)
  (let ((ch (peek-char nil input)))
    (cond
      ((char= ch #\+)
       (list :continue (read-line input)))
      ((char= ch #\*)
       (read-char input)
       (let ((cmd (%read-token input utf8)))
         (cond
           ((or (eq cmd '$OK)
                (eq cmd '$NO)
                (eq cmd '$BAD))
            ;; I assume untagged OK has only one useful argument
            ;; that's worth parsing; Dovecot appears to include some
            ;; informative text after, read that as plain text.
            (list :untagged cmd
                  (%maybe-arg input utf8)
                  (read-line input)))
           ((eq cmd '$BYE)
            (%skip-whitespace input)
            (list :untagged '$BYE (read-line input)))
           (t
            (list* :untagged
                   cmd
                   (loop for tok = (%read-token input utf8)
                         while tok collect tok))))))
      (t
       (list (%read-token input utf8)
             (%read-token input utf8)
             (%maybe-arg input utf8)
             (read-line input))))))
