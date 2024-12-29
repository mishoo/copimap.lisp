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

(defun %read-list (input eol-char)
  (loop until (char= eol-char (peek-char nil input))
        collect (prog2
                    (%skip-whitespace input)
                    (%read-token input)
                  (%skip-whitespace input))
        finally (read-char input)))

(defun %read-number (first-digit input)
  (loop with num = (- (char-code first-digit) 48)
        for ch = (peek-char nil input)
        while (char<= #\0 ch #\9)
        do (setf num (+ (* num 10)
                        (- (char-code (read-char input)) 48)))
        finally (return num)))

(defun %read-token (input)
  (%skip-whitespace input)
  (let ((ch (read-char input)))
    (cond
      ((char= #\Newline ch)
       nil)

      ((char<= #\0 ch #\9)
       (let ((num (%read-number ch input)))
         (cond
           ((char= #\: (peek-char nil input))
            (read-char input)
            (cond
              ((char= #\* (peek-char nil input))
               (read-char input)
               `(:seq ,num :*))
              ((char<= #\0 (setf ch (read-char input)) #\9)
               `(:seq ,num ,(%read-number ch input)))
              (t
               (error "Invalid sequence"))))
           (t
            num))))

      ((char= #\" ch)
       (with-output-to-string (out)
         (loop for ch = (read-char input)
               until (char= ch #\")
               do (write-char ch out))))

      ((char= #\{ ch)
       (let ((len (%read-token input)))
         (check-type len fixnum)
         (assert (char= #\} (read-char input)))
         (assert (char= #\Newline (read-char input)))
         (trivial-utf-8:read-utf-8-string (flex:flexi-stream-stream input)
                                          :byte-length len)))

      ((char= #\* ch)
       :*)

      ((char= #\( ch)
       (%read-list input #\)))

      ((char= #\[ ch)
       (%read-list input #\]))

      (t
       (cond
         ((char= #\{ (peek-char nil input)) ; literal8
          (read-char input)
          (let ((len (%read-token input)))
            (check-type len fixnum)
            (assert (char= #\} (read-char input)))
            (assert (char= #\Newline (read-char input)))
            (let ((seq (make-array len :element-type '(unsigned-byte 8))))
              (read-sequence seq (flex:flexi-stream-stream input))
              seq)))
         (t
          (let ((atom (with-output-to-string (str)
                        (write-char ch str)
                        (loop for ch = (peek-char nil input)
                              ;; XXX: should try to do a better job here.
                              until (or (char= #\Newline ch)
                                        (char= #\Space ch)
                                        (find ch "\"*%{}()" :test #'char=))
                              do (write-char (read-char input) str)))))
            (intern atom +atoms-package+))))))))

(defun %maybe-arg (input)
  (%skip-whitespace input)
  (when (char= #\[ (peek-char nil input))
    (prog1
        (%read-token input)
      (%skip-whitespace input))))

(defun %read-cmdline (input)
  (let ((ch (peek-char nil input)))
    (cond
      ((char= ch #\+)
       (list :continue (read-line input)))
      ((char= ch #\*)
       (read-char input)
       (let ((cmd (%read-token input)))
         (cond
           ((or (eq cmd '$OK)
                (eq cmd '$NO)
                (eq cmd '$BAD))
            ;; I assume untagged OK has only one useful argument
            ;; that's worth parsing; Dovecot appears to include some
            ;; informative text after, read as plain text.
            (list :untagged cmd
                  (%maybe-arg input)
                  (read-line input)))
           ((eq cmd '$BYE)
            (%skip-whitespace input)
            (list :untagged '$BYE (read-line input)))
           (t
            (list* :untagged
                   cmd
                   (loop for tok = (%read-token input)
                         while tok collect tok))))))
      (t
       (list (%read-token input)
             (%read-token input)
             (%maybe-arg input)
             (read-line input))))))
