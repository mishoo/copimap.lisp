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

(declaim (inline as-string))
(defun as-string (thing)
  (etypecase thing
    (null "NIL")
    (string thing)
    (symbol (symbol-name thing))
    (integer (format nil "~D" thing))))

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

(defun %read-token (input &optional in-seq reqid)
  (%skip-whitespace input)
  (let ((ch (read-char input)))
    (labels ((%read-atom (allow-*)
               (with-output-to-string (str)
                 (write-char ch str)
                 (loop with br = 0
                       for ch = (peek-char nil input)
                       until (or (char= #\Newline ch)
                                 (char= #\Space ch)
                                 (find ch "\\\"%{}()," :test #'char=)
                                 (and (char= #\] ch)
                                      (zerop br))
                                 (and (char= #\* ch)
                                      (not allow-*)))
                       do (case ch
                            (#\[ (incf br))
                            (#\] (decf br)))
                          (write-char (read-char input) str)))))
      (cond
        ((char= #\Newline ch)
         nil)

        ((char= #\" ch)
         (with-output-to-string (out)
           (loop for ch = (read-char input) do
             (cond
               ((char= #\\ ch)
                (write-char (read-char input) out))
               ((char= #\" ch)
                (return))
               (t
                (write-char ch out))))))

        ((char= #\{ ch)
         (let ((len (%read-token input t)))
           (check-type len fixnum)
           (assert (char= #\} (read-char input)))
           (assert (char= #\Newline (read-char input)))
           (let ((seq (make-array len :element-type '(unsigned-byte 8))))
             (read-sequence seq (flex:flexi-stream-stream input))
             (babel:octets-to-string seq :encoding :iso-8859-1))))

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

        ((char= #\\ ch)
         (intern (%read-atom t) +atoms-package+))

        (t
         (let ((atom (%read-atom nil)))
           (when reqid
             (return-from %read-token atom))
           (when (string-equal atom "NIL")
             (return-from %read-token nil))
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
             (t atom))))))))

(defun %maybe-arg (input)
  (%skip-whitespace input)
  (when (char= #\[ (peek-char nil input))
    (prog1
        (%read-token input)
      (%skip-whitespace input))))

(defun parse-imap-cmdline (input)
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
       (list (%read-token input nil t)
             (%read-token input)
             (%maybe-arg input)
             (read-line input))))))

;; 17-Jul-1996 02:44:25 -0700
(defun parse-internaldate (internaldate)
  (declare (type string internaldate))
  (rx:register-groups-bind (date month year hh mm ss offset-sign offset-hour offset-min)
      ("^ ?(\\d+)-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-(\\d+) (\\d+):(\\d+):(\\d+) ([+-])?(\\d{2})(\\d{2})$"
       internaldate)
    (declare (type string date month year hh mm ss offset-hour offset-min))
    (let* ((date (parse-integer date))
           (month (1+ (position month #("Jan" "Feb" "Mar"
                                        "Apr" "May" "Jun"
                                        "Jul" "Aug" "Sep"
                                        "Oct" "Nov" "Dec")
                                :test #'equal)))
           (year (parse-integer year))
           (hh (parse-integer hh))
           (mm (parse-integer mm))
           (ss (parse-integer ss))
           (offset-sign (if (equal offset-sign "-") -1 1))
           (offset-hour (* offset-sign (parse-integer offset-hour)))
           (offset-min (* offset-sign (parse-integer offset-min))))
      (local-time:encode-timestamp 0 ss mm hh date month year
                                   :offset (+ (* offset-hour 3600)
                                              (* offset-min 60))))))

(defun %skip-more-whitespace (input)
  (loop for ch = (peek-char nil input nil)
        while ch
        while (or (char= #\Space ch)
                  (char= #\Tab ch))
        do (read-char input)))

(defun %read-until-char (input end)
  (with-output-to-string (out)
    (loop for ch = (read-char input)
          until (char= end ch)
          do (write-char ch out))))

(defun parse-rfc822-headers (input)
  (labels
      ((peek (&optional eof-error)
         (let ((ch (peek-char nil input eof-error)))
           (cond
             ((null ch) nil)
             ((char= #\Return ch)
              (read-char input)
              (prog1
                  (if (eql #\Newline (peek-char nil input eof-error))
                      #\Newline
                      #\Return)
                (unread-char ch input)))
             (t ch))))

       (next (&optional eof-error)
         (let ((ch (read-char input eof-error)))
           (cond
             ((null ch) nil)
             ((char= #\Return ch)
              (if (eql #\Newline (setf ch (read-char input)))
                  #\Newline
                  (prog1
                      #\Return
                    (unread-char ch input))))
             (t ch))))

       (continued ()
         (let ((ch (peek)))
           (or (char= #\Space ch)
               (char= #\Tab ch))))

       (read-name ()
         (%read-until-char input #\:))

       (read-value ()
         (with-output-to-string (out)
           (loop for ch = (next) do
             (cond
               ((and (char= #\Newline ch)
                     (continued))
                (%skip-more-whitespace input)
                (write-char #\Space out))
               ((char= #\Newline ch)
                (return))
               (t
                (write-char ch out))))))

       (read-header ()
         (cons (read-name)
               (progn
                 (%skip-more-whitespace input)
                 (read-value)))))

    (loop until (char= #\Newline (peek))
          collect (read-header)
          finally (next))))

(defun decode-header (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0))
            (charset nil))
        (flet ((dump ()
                 (unless (zerop (length buffer))
                   (write-string (babel:octets-to-string buffer :encoding charset) out)
                   (setf (fill-pointer buffer) 0)))

               (read-chunk ()
                 (%read-until-char in #\?))

               (hex-byte (c1 c2)
                 (declare (type character c1 c2))
                 (parse-integer (format nil "~C~C" c1 c2)
                                :radix 16)))
          (declare (inline dump read-chunk hex-byte))
          (loop with in-q = nil
                for ch = (read-char in nil)
                while ch do
                  (cond
                    (in-q (cond
                            ((char= #\_ ch)
                             (dump)
                             (write-char #\Space out))

                            ((char= #\? ch)
                             (assert (char= #\= (read-char in)))
                             (dump)
                             (setf in-q nil)
                             (%skip-more-whitespace in))

                            ((char= #\= ch)
                             (vector-push-extend (hex-byte (read-char in)
                                                           (read-char in))
                                                 buffer))

                            (t
                             (dump)
                             (write-char ch out))))

                    ((and (char= #\= ch)
                          (eql #\? (peek-char nil in nil)))
                     (read-char in)
                     (setf charset (intern (string-upcase (read-chunk)) :keyword))
                     (ecase (read-char in)
                       ((#\q #\Q)
                        (assert (char= #\? (read-char in)))
                        (setf in-q t))

                       ((#\b #\B)
                        (assert (char= #\? (read-char in)))
                        (write-string (babel:octets-to-string
                                       (base64:base64-string-to-usb8-array (read-chunk))
                                       :encoding charset)
                                      out)
                        (assert (char= #\= (read-char in)))
                        (%skip-more-whitespace in))))

                    (t
                     (write-char ch out)))))))))

(defun encode-header (str)
  (let ((bytes (trivial-utf-8:string-to-utf-8-bytes str)))
    (if (/= (length bytes) (length str))
        (format nil "=?utf-8?b?~A?="
                (cl-base64:usb8-array-to-base64-string bytes))
        str)))

(defun get-header (headers name)
  (cdr (assoc name headers :test #'equalp)))

(defun write-rfc822-headers (headers &optional (output t))
  (loop with crlf = #.(coerce #(#\Return #\Newline) 'string)
        for (key . val) in headers do
          (write-string (as-string key) output)
          (write-string ": " output)
          (write-string val output)
          (write-string crlf output)
        finally (write-string crlf output)))
