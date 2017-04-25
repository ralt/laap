(in-package #:laap)

(defclass timer ()
  ((direction :initarg :direction :reader direction)
   (callback :initarg :callback :reader callback)))

(defmethod handle-error ((timer timer) error)
  (funcall (callback timer) error nil))

(defmethod handle-event ((timer timer) fd)
  )

(defclass event-loop ()
  ((timers :accessor timers)))

(defmethod initialize-instance ((loop event-loop) &key)
  (setf (timers loop) (make-hash-table)))

(defun start (loop)
  ;; TODO: Don't hardcode 64.
  (let ((events (cffi:foreign-alloc '(:struct epoll-event) :count 64)))
    (unwind-protect
	 (let ((efd (epoll-create1 0)))
	   (when (= efd -1)
	     (error "epoll_create1"))
	   (unwind-protect
		(progn
		  (maphash
		   (lambda (timerfd timer)
		     (cffi:with-foreign-object (event '(:struct epoll-event))
		       (setf (cffi:foreign-slot-value
			      (cffi:foreign-slot-value event '(:struct epoll-event) 'data)
			      'epoll-data-t
			      'fd)
			     timerfd)
		       (setf (cffi:foreign-slot-value event '(:struct epoll-event) 'events)
			     (logior (direction timer) +epollet+ +epolloneshot+))
		       (when (= (epoll-ctl efd +epoll-ctl-add+ timerfd event) -1)
			 (error "epoll_ctl"))))
		   (timers loop))
		  (main-loop loop efd events))
	     (c-close efd)))
      (cffi:foreign-free events))))

(defun main-loop (loop efd events)
  (loop
     (when (= (length (timers loop)) 0)
       (return-from main-loop))
     (loop for i from 0 to (1- (epoll-pwait efd events 64 -1 0))
	do (block continue
	     (let* ((event (cffi:mem-aref events '(:struct epoll-event) i))
		    (event-events (cffi:foreign-slot-value event '(:struct epoll-event) 'events))
		    (event-data (cffi:foreign-slot-value event '(:struct epoll-event) 'data))
		    (fd (cffi:foreign-slot-value event-data 'epoll-data-t 'fd))
		    (timer (gethash fd (timers loop))))
	      (when (or (> (logand event-events +epollerr+) 0)
			(> (logand event-events +epollhup+) 0))
		(return-from continue
		  (handle-error timer (make-condition 'error "epoll error"))))
	      (handle-event timer fd)
	      ;; Attach back the event given that we're using EPOLLONESHOT.
	      )))))

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
