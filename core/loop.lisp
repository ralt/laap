(in-package #:laap)

;;; TODO: figure out how to improve the locked hash table.

;; A reasonable default, overridden by the loop.
(defvar *recv-buffer-length* 4096)

(defclass event-loop ()
  ((timers :accessor timers)
   (timers-lock :accessor timers-lock)
   (efd :accessor efd)
   (started :accessor started)))

(defmethod initialize-instance ((loop event-loop) &key)
  (setf (timers loop) (make-hash-table))
  (setf (timers-lock loop) (bt:make-lock))
  (setf (started loop) nil))

(defun find-recv-buffer-length ()
  (with-open-file (f "/proc/sys/net/core/rmem_default")
    (parse-integer (read-line f))))

(defun cores-count ()
  (if (uiop:getenvp "LAAP_MAX_LOOPS")
      ;; Manual override
      (parse-integer (uiop:getenv "LAAP_MAX_LOOPS"))
      (with-open-file (f "/proc/cpuinfo")
	(let ((count 0))
	  (loop for line = (read-line f nil 'eof)
	     until (eq line 'eof)
	     when (ppcre:scan "^cpu cores" line)
	     do (incf count))
	  count))))

(defun start-event-loops ()
  (let* ((efd (epoll-create1 +epoll-cloexec+))
	 (threads-count (cores-count)))
    (when (= efd -1)
      (error "epoll_create1"))
    (setf (efd *loop*) efd)
    (maphash
     (lambda (timerfd timer)
       (add-event efd timerfd timer))
     (timers *loop*))
    (setf (started *loop*) t)
    (setf (max-event-loops *thread-pool*) threads-count)
    (loop for i below threads-count
       do (add-thread (lambda ()
			(main-loop efd))))))

(defun exit-event-loop-p ()
  (when *thread-should-exit*
    (return-from exit-event-loop-p t))
  (bt:with-lock-held ((timers-lock *loop*))
    (when (= (hash-table-count (timers *loop*)) 0)
      (quit-event-loop))))

(defun main-loop (efd)
  (let ((events (cffi:foreign-alloc '(:struct epoll-event) :count 1)))
    (unwind-protect
	 (loop
	    (when (exit-event-loop-p) (return-from main-loop))
	    (let ((n (epoll-wait efd events 1 -1)))
	      (when (= n 1)
		(block continue
		  (let* ((event (cffi:mem-aref events '(:struct epoll-event) 0))
			 (event-events (getf event 'events))
			 (fd (ldb (byte 32 0) (getf event 'data)))
			 (timer (bt:with-lock-held ((timers-lock *loop*))
				  (gethash fd (timers *loop*)))))
		    (when (or (> (logand event-events +epollerr+) 0)
			      (> (logand event-events +epollhup+) 0))
		      (unwind-protect
			   (handle-error
			    timer
			    (make-condition
			     'epoll-error
			     :error-type (cond ((not (= (logand event-events +epollerr+) 0)) :err)
					       ((not (= (logand event-events +epollhup+) 0)) :hup)
					       ((not (= (logand event-events +epollpri+) 0)) :pri))))
			(when (exit-event-loop-p) (return-from main-loop))))

		    (handle-event timer)

		    (when (exit-event-loop-p) (return-from main-loop)))))))
      (cffi:foreign-free events))))

(defun add-event (efd timerfd timer)
  (handler-case
      (%add-event efd timerfd (direction timer))
    (error (e)
      (handle-error timer e))))

(defun %add-event (efd fd direction)
  (cffi:with-foreign-object (event '(:struct epoll-event))
    (setf (cffi:foreign-slot-value event '(:struct epoll-event) 'data) fd)
    (setf (cffi:foreign-slot-value event '(:struct epoll-event) 'events)
	  (logior direction +epollet+ +epolloneshot+))
    (when (= (epoll-ctl efd +epoll-ctl-add+ fd event) -1)
      (unless (= errno +eexist+)
	(error (make-condition 'os-error :errno errno)))
      (epoll-ctl efd +epoll-ctl-mod+ fd event))))
