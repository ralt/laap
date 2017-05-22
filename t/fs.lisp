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

(defun temporary-file ()
  (let ((f (format nil "/tmp/~a" (random-string))))
    (close (open f :direction :probe :if-does-not-exist :create))
    f))

(test file-read-empty (done)
  (let ((file (make-instance 'laap/fs:file
			     :path (temporary-file)
			     :direction :input)))
    (laap/fs:read
     file
     (lambda (err res)
       (when err (error err))
       (assert (= (length res) 0))
       (funcall done))
     :count 4096)))

(defvar *temporary-file-foo* (format nil "/tmp/~a" (random-string)))
(with-open-file (f *temporary-file-foo*
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :overwrite)
  (format f "foo~%"))

(test file-read (done)
  (let ((file (make-instance 'laap/fs:file
			     :path *temporary-file-foo*
			     :direction :input)))
    (laap/fs:read
     file
     (lambda (err res)
       (when err (error err))
       (assert (= (length res) 4))
       (assert (string= (babel:octets-to-string res) (format nil "foo~%")))
       (format t "~a~%" (babel:octets-to-string res))
       (laap/fs:close file (lambda (err res)
			     (declare (ignore err res))
			     (funcall done))))
     :count 4096)))

(test file-write (done)
  (let* ((temp (temporary-file))
	 (file (make-instance 'laap/fs:file
			      :path temp
			      :direction :output
			      :if-does-not-exist :create)))
    (laap/fs:write
     file
     (lambda (err res)
       (declare (ignore res))
       (when err (error err))
       (assert (string= (with-open-file (f temp)
			  (read-line f))
			(format nil "foo~%")))
       (laap/fs:close file (lambda (err res)
			     (declare (ignore err res))
			     (funcall done))))
     :data (babel:string-to-octets (format nil "foo~%")))))

(test file-rename (done)
  (let ((temp (temporary-file))
	(new-name (format nil "/tmp/~a" (random-string))))
    (laap/fs:rename
     (lambda (err res)
       (declare (ignore res))
       (when err (error err))
       (assert (probe-file new-name))
       (funcall done))
     :oldpath temp :newpath new-name)))
