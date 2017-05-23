(in-package #:laap/fs)

(defun close (file callback)
  (laap:with-blocking-thread close
    (if (= (c-close (fd file)) 0)
	(funcall callback nil nil)
	(funcall callback (strerror errno) nil))))

(deftype direction ()
  '(member :input :output :input-output))

(deftype if-does-not-exist ()
  '(member :create))

(defclass file ()
  ((path :initarg :path :accessor path)
   (fd :accessor fd)
   (direction :initarg :direction :reader direction :type direction)
   (if-does-not-exist :initarg :if-does-not-exist :accessor if-does-not-exist :type if-does-not-exist)))

(defmethod initialize-instance :after ((file file) &key)
  (let ((open-flags (logior (cond ((eq (direction file) :output) +o-write-only+)
				  ((eq (direction file) :input) +o-read-only+)
				  ((eq (direction file) :input-output) +o-read-write+)
				  (t 0))
			    (cond ((not (slot-boundp file 'if-does-not-exist)) 0)
				  ((eq (if-does-not-exist file) :create) +o-create+)
				  (t 0)))))
    (laap:with-blocking-thread open
      (loop
	 (let ((fd (c-open (path file) open-flags)))
	   (if (= fd -1)
	       (unless (= errno +eintr+)
		 (error (strerror errno)))
	       (return-from open (setf (fd file) fd))))))))

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

(defun truncate (file callback &key length)
  (laap:with-blocking-thread truncate
    (loop
       (if (= (c-ftruncate (fd file) length) -1)
	   (unless (= errno +eintr+)
	     (return-from truncate (funcall callback (strerror errno) nil)))
	   (return-from truncate (funcall callback nil nil))))))

(defun link (callback &key oldpath newpath)
  (laap:with-blocking-thread rename
    (cffi:with-foreign-strings ((c-oldpath oldpath)
				(c-newpath newpath))
      (if (= (c-link c-oldpath c-newpath) 0)
	  (funcall callback nil nil)
	  (funcall callback (strerror errno) nil)))))
