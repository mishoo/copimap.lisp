(in-package #:imapsync)

(defconstant +atoms-package+ (find-package "IMAPSYNC-ATOMS"))

(in-readtable imapsync::syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character
   #\$ (lambda (stream ch)
         (declare (ignore ch))
         (let ((*package* +atoms-package+)
               (*readtable* *preserve-case-readtable*))
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

(defun %read-token (input &optional in-seq)
  (%skip-whitespace input)
  (let ((ch (read-char input)))
    (cond
      ((char= #\Newline ch)
       nil)

      ((char= #\" ch)
       (with-output-to-string (out)
         (loop for ch = (read-char input)
               until (char= ch #\")
               do (write-char ch out))))

      ((char= #\{ ch)
       (let ((len (%read-token input t)))
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

      ((and (char= #\~ ch)
            (char= #\{ (peek-char nil input))) ; literal8
       (read-char input)
       (let ((len (%read-token input t)))
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
                                     (find ch "\"*%{}()," :test #'char=)
                                     (and (char= #\] ch)
                                          (zerop br)))
                           do (case ch
                                (#\[ (incf br))
                                (#\] (decf br)))
                              (write-char (read-char input) str)))))
         (setf atom
               (block nil
                 (rx:register-groups-bind (a colon b) ("^(\\d+)(:)?(\\d+)?$" atom)
                   (unless colon (return (parse-integer a)))
                   (when b
                     (return `(:range ,(parse-integer a)
                                      ,(parse-integer b))))
                   (when (eql #\* (peek-char nil input))
                     (read-char input)
                     (return `(:range ,(parse-integer a) :*))))
                 (intern atom +atoms-package+)))
         (cond
           (in-seq atom)
           ((eql #\, (peek-char nil input))
            `(:seq ,atom
                   ,@(loop do (read-char input)
                           collect (%read-token input t)
                           while (eql #\, (peek-char nil input)))))
           (t atom)))))))

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
            ;; informative text after, read that as plain text.
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
