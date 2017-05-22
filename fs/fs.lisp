(in-package #:laap/fs)

(defun open (path callback &key (flags '(+o-read-only+)))
  (let ((open-flags (reduce (lambda (p c)
			      (logior p c))
			    flags)))
    (format t "open flags: ~a~%" open-flags)
    (laap:with-blocking-thread open
      (loop
	 (let ((fd (c-open path open-flags)))
	   (if (= fd -1)
	       (unless (= errno +eintr+)
		 (funcall callback (strerror errno) nil))
	       (return-from open (funcall callback nil (make-instance 'file :fd fd)))))))))

(defun close (file callback)
  (laap:with-blocking-thread close
    (if (= (c-close (fd file)) 0)
	(funcall callback nil nil)
	(funcall callback (strerror errno) nil))))

(defclass file ()
  ((fd :initarg :fd :reader fd)))

(defun read (file callback &key count)
  (laap:with-blocking-thread read
    (let ((lisp-buffer (make-array count :element-type '(unsigned-byte 8)))
	  (total-read-bytes 0))
      (loop
	 (cffi:with-foreign-object (buf :char count)
	   (let ((read-bytes (c-read (fd file) buf (- count total-read-bytes))))
	     (when (= read-bytes -1)
	       (unless (= errno +eintr+)
		 (return-from read (funcall callback (strerror errno) nil))))
	     (when (= read-bytes 0)
	       ;; EOF
	       (return))
	     (c-buffer-to-lisp-buffer buf lisp-buffer read-bytes total-read-bytes)
	     (when (= (+ total-read-bytes read-bytes) count)
	       (return))
	     (incf total-read-bytes read-bytes))))
      (let ((return-value (make-array total-read-bytes
				      :element-type '(unsigned-byte 8))))
	(loop for i below total-read-bytes
	   do (setf (elt return-value i) (elt lisp-buffer i)))
	(funcall callback nil return-value)))))

(defun write (file callback &key data)
  (laap:with-blocking-thread write
    (let ((data-length (length data))
	  (total-written-bytes 0))
      (loop
	 (let ((buffer-length (- data-length total-written-bytes)))
	   (cffi:with-foreign-object (buf :char buffer-length)
	     (lisp-buffer-to-c-buffer data buf buffer-length total-written-bytes)
	     (let ((written-bytes (c-write (fd file) buf data-length)))
	       (when (= written-bytes -1)
		 (unless (= errno +eintr+)
		   (return-from write (funcall callback (strerror errno) nil))))
	       (when (= (+ written-bytes total-written-bytes) data-length)
		 (return-from write (funcall callback nil nil)))
	       (incf total-written-bytes written-bytes))))))))

(defun c-buffer-to-lisp-buffer (c-buffer lisp-buffer length offset)
  (loop for i below length
     do (setf (elt lisp-buffer (+ offset i)) (cffi:mem-aref c-buffer :char i))))

(defun lisp-buffer-to-c-buffer (lisp-buffer c-buffer length offset)
  (loop for i below length
     do (setf (cffi:mem-aref c-buffer :char i) (elt lisp-buffer (+ offset i)))))

(defun rename (callback &key oldpath newpath)
  (laap:with-blocking-thread rename
    (cffi:with-foreign-strings ((c-oldpath oldpath)
				(c-newpath newpath))
      (if (= (c-rename c-oldpath c-newpath) 0)
	  (funcall callback nil nil)
	  (funcall callback (strerror errno) nil)))))
