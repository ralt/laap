(in-package #:laap)

(defclass timer ()
  ((fd :initarg :fd :reader fd)
   (direction :initarg :direction :reader direction)
   (callback :initarg :callback :reader callback)
   (closed :initform nil :accessor closed))
  (:documentation "A timer is an object whose file descriptor is added
  to epoll, used to maintain the mapping between the file descriptor
  and the callback, at the minimum. Other fields can be used for
  further synchronization and bookkeeping matters."))

(defgeneric handle-error (timer err)
  (:documentation "Handles an error for a timer."))

(defgeneric handle-event (timer)
  (:documentation "Handles an event for a timer."))

(defgeneric add-timer (timer)
  (:documentation "Add a timer to the event loop."))

(defgeneric remove-timer (timer)
  (:documentation "Remove a timer from the event loop."))

(defmethod add-timer (timer)
  (bt:with-lock-held ((timers-lock *loop*))
    (setf (gethash (fd timer) (timers *loop*)) timer))
  (when (started *loop*)
    (add-event (efd *loop*) (fd timer) timer)))

(defmethod remove-timer (timer)
  (bt:with-lock-held ((timers-lock *loop*))
    (unless (eq (gethash (fd timer) (timers *loop*))
		timer)
      (return-from remove-timer))
    (setf (closed timer) t)
    (remhash (fd timer) (timers *loop*))))

(defclass timer-timer (timer)
  ((direction :initform +epollin+))
  (:documentation "A POSIX timer-based timer. Used to run callbacks in
  a given amount of time."))

(defmethod handle-error ((timer timer-timer) error)
  (unwind-protect
       (funcall (callback timer) error nil)
    (progn
      (remove-timer timer)
      (c-close timer))))

(defmethod handle-event ((timer timer-timer))
  ;; We don't really need to read the timer fd, once we get
  ;; an event, it can only mean that it's ready.
  (unwind-protect
       (funcall (callback timer) nil nil)
    (progn
      (remove-timer timer)
      (c-close (fd timer)))))

(defun add-timer-in (seconds callback)
  (let ((timerfd (timerfd-create +clock-monotonic+ +tfd-cloexec+)))
    (when (= timerfd -1)
      (error (make-condition 'os-error :errno errno)))
    (multiple-value-bind (integer remaining)
	(floor seconds)
      (cffi:with-foreign-objects ((value-timespec '(:struct timespec))
				  (new-value '(:struct itimerspec))
				  (interval-timespec '(:struct itimerspec)))
	(setf (cffi:foreign-slot-value value-timespec '(:struct timespec) 'tv-sec) integer)
	(setf (cffi:foreign-slot-value value-timespec '(:struct timespec) 'tv-nsec)
	      (round (* remaining 1000000)))
	(setf (cffi:foreign-slot-value new-value '(:struct itimerspec) 'it-value)
	      value-timespec)

	(setf (cffi:foreign-slot-value interval-timespec '(:struct timespec) 'tv-sec) 0)
	(setf (cffi:foreign-slot-value interval-timespec '(:struct timespec) 'tv-nsec) 0)
	(setf (cffi:foreign-slot-value new-value '(:struct itimerspec) 'it-interval)
	      interval-timespec)

	(timerfd-settime timerfd 0 new-value (cffi:null-pointer))))
    (add-timer (make-instance 'timer-timer :fd timerfd :callback callback))))

(defun delay (seconds callback)
  "Run a callback in a specified amount of seconds."
  (add-timer-in seconds callback))

(defun spawn (callback)
  "Immediately run a callback."
  ;; 1 microsecond is the minimum value a timerfd needs.
  (add-timer-in 0.000001 callback))
