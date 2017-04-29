(in-package #:laap)

(defclass timer ()
  ((fd :initarg :fd :reader fd)
   (direction :initarg :direction :reader direction)
   (callback :initarg :callback :reader callback)
   (closed :initform nil :accessor closed)))

(defmethod handle-error ((timer timer) error)
  (let ((*error* error))
    (funcall (callback timer))))

(defgeneric handle-event (timer loop)
  (:documentation "Handles an event for a file descriptor"))

(defclass timer-timer (timer)
  ((direction :initform +epollin+)))

(defmethod handle-event ((timer timer-timer) loop)
  ;; We don't really need to read the timer fd, once we get
  ;; an event, it can only mean that it's ready.
  (unwind-protect
       (funcall (callback timer))
    (progn
      (setf (closed timer) t)
      (remhash (fd timer) (timers loop))
      (c-close (fd timer)))))

(defclass event-loop ()
  ((started :accessor started)
   (timers :accessor timers)
   (efd :accessor efd)))

(defmethod initialize-instance ((loop event-loop) &key)
  (setf (timers loop) (make-hash-table))
  (setf (started loop) nil))

(defun cores-count ()
  (if (uiop:getenvp "LAAP_MAX_LOOPS")
      ;; Manual override
      (parse-integer (uiop:getenv "LAAP_MAX_LOOPS"))
      (with-open-file (f "/proc/cpuinfo")
	(let ((count 0))
	  (loop for line = (read-line f nil 'eof)
	     until (eq line 'eof)
	     when (ppcre:scan "^cpu cores" line)
	     do (incf count (parse-integer (second (ppcre:split ":" line)))))
	  count))))

(defun start (loop)
  (let ((efd (epoll-create1 0)))
    (when (= efd -1)
      (error "epoll_create1"))
    (setf (efd loop) efd)
    (maphash
     (lambda (timerfd timer)
       (add-event efd timerfd timer))
     (timers loop))
    (setf (started loop) t)
    (let* ((threads-count (cores-count))
	   (threads (loop for i below threads-count
		       collect (bt:make-thread
				(lambda ()
				  (main-loop loop efd))
				:name (format nil "laap event loop ~a" i)))))
      (loop for thread in threads
	 do (bt:join-thread thread)))))

(defun main-loop (loop efd)
  (let ((events (cffi:foreign-alloc '(:struct epoll-event) :count 1)))
    (unwind-protect
	 (loop
	    (when (= (hash-table-count (timers loop)) 0)
	      (return-from main-loop))
	    (let ((n (epoll-wait efd events 1 -1)))
	      (loop for i below n
		 do (block continue
		      (let* ((event (cffi:mem-aref events '(:struct epoll-event) i))
			     (event-events (getf event 'events))
			     (fd (ldb (byte 32 0) (getf event 'data)))
			     (timer (gethash fd (timers loop))))
			(when (or (> (logand event-events +epollerr+) 0)
				  (> (logand event-events +epollhup+) 0)
				  (not (logand event-events (direction timer))))
			  (unwind-protect
			       (handle-error timer (make-condition 'error "epoll error"))
			    (if (= (hash-table-count (timers loop)) 0)
				(return-from main-loop)
				(return-from continue))))
			(handle-event timer loop)
			(unless (closed timer)
			  (epoll-ctl efd +epoll-ctl-mod+ fd event))
			(when (= (hash-table-count (timers loop)) 0)
			  (setf (started loop) nil)
			  (c-close efd)
			  (return-from main-loop)))))))
      (cffi:foreign-free events))))

(defun add-timer (loop timer)
  (setf (gethash (fd timer) (timers loop)) timer)
  (when (started loop)
    (add-event (efd loop) (fd timer) timer)))

(defun add-event (efd timerfd timer)
  (cffi:with-foreign-object (event '(:struct epoll-event))
    (setf (cffi:foreign-slot-value event '(:struct epoll-event) 'data) timerfd)
    (setf (cffi:foreign-slot-value event '(:struct epoll-event) 'events)
	  (logior (direction timer) +epollet+ +epolloneshot+))
    (when (= (epoll-ctl efd +epoll-ctl-add+ timerfd event) -1)
      (error "epoll_ctl"))))
