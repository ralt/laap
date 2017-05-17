(in-package #:laap/test)

(defvar *ascii-alphabet* "abcdefghijklmnopqrstuvwxyz")

;;; https://common-lisp.net/project/bese/docs/arnesi/html/api/function_005FIT.BESE.ARNESI_003A_003ARANDOM-STRING.html
(defun random-string (&optional (length 32) (alphabet *ascii-alphabet*))
  "Returns a random alphabetic string.

The returned string will contain LENGTH characters chosen from
the vector ALPHABET.
"
  (loop with id = (make-string length)
	with alphabet-length = (length alphabet)
	for i below length
	do (setf (cl:aref id i)
		 (cl:aref alphabet (random alphabet-length)))
	finally (return id)))

(defvar *temporary-empty-file* (format nil "/tmp/~a" (random-string)))
(open *temporary-empty-file* :direction :probe :if-does-not-exist :create)

(test file-read-empty (done)
  (laap/fs:open
   *temporary-empty-file*
   (lambda (err file)
     (when err (error err))
     (laap/fs:read
      file
      (lambda (err res)
	(when err (error err))
	(assert (= (length res) 0))
	(funcall done))
      :count 4096))))

(defvar *temporary-file* (format nil "/tmp/~a" (random-string)))
(with-open-file (f *temporary-file*
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :overwrite)
  (format f "foo~%"))

(test file-read (done)
  (laap/fs:open
   *temporary-file*
   (lambda (err file)
     (when err (error err))
     (laap/fs:read
      file
      (lambda (err res)
	(when err (error err))
	(assert (= (length res) 4))
	(assert (string= (babel:octets-to-string res) (format nil "foo~%")))
	(format t "~a~%" (babel:octets-to-string res))
	(funcall done))
      :count 4096))))
