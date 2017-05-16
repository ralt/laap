(in-package #:laap/fs)

(defun open (path callback)
  (laap:with-blocking-thread open
    (loop
       (let ((fd (c-open path 0)))
	 (if (= fd -1)
	     (unless (= errno +eintr+)
	       (funcall callback (strerror errno) nil))
	     (return-from open (funcall callback nil (make-instance 'file :fd fd))))))))

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

(defun c-buffer-to-lisp-buffer (c-buffer lisp-buffer length offset)
  (loop for i below length
     do (setf (elt lisp-buffer (+ offset i)) (cffi:mem-aref c-buffer :char i))))
