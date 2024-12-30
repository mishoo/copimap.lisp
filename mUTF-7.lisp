(in-package #:imapsync)

(defparameter *mod-b64-chars* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,")

(defparameter *mod-b64-chars-reverse*
  (let ((a (make-array 256 :initial-element nil)))
    (loop for ch across *mod-b64-chars*
          for pos from 0
          do (setf (aref a (char-code ch)) pos))
    a))

(defun mutf-7-to-string (wtf)
  (with-input-from-string (in wtf)
    (with-output-to-string (out)
      (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
        (labels ((enc (ch)
                   (aref *mod-b64-chars-reverse* (char-code ch)))
                 (next-enc ()
                   (unless (eql #\- (peek-char nil in nil))
                     (enc (read-char in)))))
          (declare (inline enc next-enc))
          (loop with esc = nil
                for ch = (read-char in nil)
                while ch do
                  (cond
                    ((and esc (eql #\- ch))
                     (write-string (babel:octets-to-string buffer :encoding :utf-16/be) out)
                     (setf esc nil (fill-pointer buffer) 0))
                    (esc
                     (let ((enc1 (enc ch))
                           (enc2 (next-enc))
                           (enc3 (next-enc))
                           (enc4 (next-enc)))
                       (vector-push-extend (logior (ash enc1 2)
                                                   (ash (or enc2 0) -4))
                                           buffer)
                       (when enc3
                         (vector-push-extend (logior (ash (logand enc2 15) 4)
                                                     (ash enc3 -2))
                                             buffer)
                         (when enc4
                           (vector-push-extend (logior (ash (logand enc3 3) 6)
                                                       enc4)
                                               buffer)))))
                    ((eql #\& ch)
                     (cond
                       ((eql #\- (peek-char nil in))
                        (read-char in)
                        (write-char #\& out))
                       (t
                        (setf esc t))))
                    (t
                     (write-char ch out)))))))))

(defun string-to-mutf-7 (str)
  (with-input-from-string (in str)
    (with-output-to-string (out)
      (let ((buffer (make-array 0 :element-type 'character
                                  :adjustable t :fill-pointer 0)))
        (labels ((enc (index)
                   (write-char (aref *mod-b64-chars* index) out))
                 (dump ()
                   (unless (zerop (length buffer))
                     (let ((bytes (babel:string-to-octets buffer :encoding :utf-16/be)))
                       (write-char #\& out)
                       (loop with n = (length bytes)
                             for i below n
                             for chr1 = (aref bytes (prog1 i (incf i)))
                             for chr2 = (when (< i n)
                                          (aref bytes (prog1 i (incf i))))
                             for chr3 = (when (< i n)
                                          (aref bytes i))
                             do (enc (ash chr1 -2))
                                (enc (logior (ash (logand chr1 3) 4)
                                             (ash (or chr2 0) -4)))
                                (when chr2
                                  (enc (logior (ash (logand chr2 15) 2)
                                               (ash (or chr3 0) -6)))
                                  (when chr3
                                    (enc (logand chr3 63)))))
                       (write-char #\- out))
                     (setf (fill-pointer buffer) 0))))
          (loop for ch = (read-char in nil) while ch do
            (cond
              ((eql ch #\&)
               (dump)
               (write-string "&-" out))
              ((<= #x20 (char-code ch) #x7e)
               (dump)
               (write-char ch out))
              (t
               (vector-push-extend ch buffer)))
                finally (dump)))))))
