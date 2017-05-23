(in-package #:laap/test)

(defun getrandom (count)
  (let ((lisp-array (make-array count))
	(read-bytes 0))
    (loop
       (cffi:with-foreign-object (buf :char (- count read-bytes))
	 (let ((n (cffi:foreign-funcall "syscall" :long 318 :pointer buf :int count :uint 0 :long)))
	   (if (= n -1)
	       (unless (= laap/fs::errno laap/fs::+eintr+)
		 (error (laap/fs::strerror laap/fs::errno)))
	       (progn
		 (loop for i below count do (setf (elt lisp-array (+ i read-bytes))
						  (cffi:mem-aref buf :char i)))
		 (incf read-bytes n)
		 (when (= read-bytes count)
		   (return-from getrandom lisp-array)))))))))

(defun random-string ()
  (remove "=" (base32:bytes-to-base32 (getrandom 16))
	  :test #'string=))

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

(test file-truncate (done)
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
       (with-open-file (f temp)
	 (assert (= (file-length f) 3)))
       (laap/fs:truncate
	file
	(lambda (err res)
	  (declare (ignore res))
	  (when err (error err))
	  (with-open-file (f temp)
	    (assert (= (file-length f) 1)))
	  (funcall done))
	:length 1))
     :data (babel:string-to-octets "foo"))))

(test file-link (done)
  (let ((temp (temporary-file))
	(new-name (format nil "/tmp/~a" (random-string))))
    (laap/fs:link
     (lambda (err res)
       (declare (ignore res))
       (when err (error err))
       (assert (probe-file new-name))
       (funcall done))
     :oldpath temp :newpath new-name)))

(test file-symlink (done)
  (let ((temp (temporary-file))
	(new-name (format nil "/tmp/~a" (random-string))))
    (laap/fs:symlink
     (lambda (err res)
       (declare (ignore res))
       (when err (error err))
       (assert (probe-file new-name))
       (funcall done))
     :target temp :linkpath new-name)))
