(in-package #:laap)

(defclass timer ()
  ((fd :initarg :fd :reader fd)
   (direction :initarg :direction :reader direction)
   (callback :initarg :callback :reader callback)
   (closed :initform nil :accessor closed)))

(defmethod handle-error ((timer timer) error)
  (funcall (callback timer) error nil))

(defgeneric handle-event (timer)
  (:documentation "Handles an event for a file descriptor"))

(defclass timer-timer (timer)
  ((direction :initform +epollin+)))

(defmethod handle-event ((timer timer-timer))
  ;; We don't really need to read the timer fd, once we get
  ;; an event, it can only mean that it's ready.
  (funcall (callback timer) nil)
  (setf (closed timer) t)
  (c-close (fd timer)))

(defclass event-loop ()
  ((started :accessor started)
   (timers :accessor timers)
   (efd :accessor efd)))

(defmethod initialize-instance ((loop event-loop) &key)
  (setf (timers loop) (make-hash-table))
  (setf (started loop) nil))

(defun start (loop)
  ;; TODO: Don't hardcode 64.
  (let ((events (cffi:foreign-alloc '(:struct epoll-event) :count 64)))
    (unwind-protect
	 (let ((efd (epoll-create1 0)))
	   (when (= efd -1)
	     (error "epoll_create1"))
	   (setf (efd loop) efd)
	   (unwind-protect
		(progn
		  (maphash
		   (lambda (timerfd timer)
		     (add-event efd timerfd timer))
		   (timers loop))
		  (setf (started loop) t)
		  (main-loop loop efd events))
	     (c-close efd)))
      (cffi:foreign-free events))))

(defun main-loop (loop efd events)
  (loop
     (when (= (hash-table-count (timers loop)) 0)
       (setf (started loop) nil)
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
	      (unwind-protect
		   (handle-event timer)
		(unless (closed timer)
		  (epoll-ctl efd +epoll-ctl-mod+ fd event))))))))

(defun add-timer (loop timer)
  (setf (gethash (fd timer) (timers loop)) timer)
  (when (started loop)
    (add-event (efd loop) (fd timer) timer)))

(defun add-event (efd timerfd timer)
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
