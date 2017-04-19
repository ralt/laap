(in-package #:laap)

(defclass timer ()
  ((fire-time :initarg :fire-time :reader fire-time)
   (callback :initarg :callback :reader callback)))

(defclass event-loop ()
  ((timers :initform nil :accessor timers)))

(defun start (loop)
  (loop
     (let ((next-timeout (run-timers loop)))
       (unless next-timeout
	 (return))
       (sleep next-timeout))))

(defun run-timers (loop)
  (let ((now (gettimeofday)))
    (loop
       (if (timers loop)
	   (let ((timer (first (timers loop))))
	     (if (< (fire-time timer) now)
		 (progn
		   (pop (timers loop))
		   (funcall (callback timer)))
		 (return)))
	   (return)))
    (when (timers loop)
      (- (fire-time (first (timers loop))) now))))

(defun insert-before (list index new-element)
  (if (= index 0)
      (push new-element list)
      (push new-element (cdr (nthcdr (1- index) list)))))

(defun add-callback (loop fire-time callback)
  (let ((new-timer (make-instance 'timer
				  :fire-time fire-time
				  :callback callback)))
    (if (timers loop)
	(loop for i from 0 upto (1- (length (timers loop)))
	   do (progn
		(when (< fire-time (fire-time (nth i (timers loop))))
		  (insert-before (timers loop) i new-timer)
		  (return))
		(when (= i (1- (length (timers loop))))
		  ;; Oops, we're the last item.
		  (setf (timers loop) (append (timers loop) (list new-timer))))))
	(push new-timer (timers loop)))))
