(in-package #:laap)

(defvar *loop* nil)

(defmacro with-event-loop (&body body)
  `(progn
     (setf *loop* (make-instance 'event-loop))
     (progn
       (progn ,@body)
       (start *loop*))))

(defmacro with-magic (&body body)
  (let ((initial-laap (gensym)))
    `(with-event-loop
       (deflaap ,initial-laap ()
	 ,@body)
       (,initial-laap))))

(defmacro deflaap (name args &body body)
  (let ((coroutine (gensym)))
    `(let ((self nil))
       (cl-coroutine:defcoroutine ,coroutine ,args
	 ,@body)
       (setf self (cl-coroutine:make-coroutine ',coroutine))
       (defun ,name (&rest arguments)
	 (apply self arguments)))))

(defmacro defpublic (name args &body body)
  (let ((function-name (intern (concatenate 'string "laap-public-" (symbol-name name)))))
    `(progn
       (export ',function-name)
       (export ',name)
       (defun ,function-name ,args
	 ,@body)
       (defmacro ,name ,(cdr (reverse args))
	 (list
	  'cl-coroutine:yield
	  (list ',function-name ,@(cdr (reverse args)) 'laap:self))))))

(defpublic delay (seconds callback)
  (let ((timerfd (timerfd-create +clock-realtime+ 0)))
    (multiple-value-bind (integer remaining)
	(floor seconds)
      (cffi:with-foreign-objects ((value-timespec '(:struct timespec))
				  (new-value '(:struct itimerspec))
				  (interval-timespec '(:struct itimerspec)))
	(setf (cffi:foreign-slot-value value-timespec '(:struct timespec) 'tv-sec) integer)
	(setf (cffi:foreign-slot-value value-timespec '(:struct timespec) 'tv-nsec)
	      (* remaining 1000000))
	(setf (cffi:foreign-slot-value new-value '(:struct itimerspec) 'it-value)
	      value-timespec)

	(setf (cffi:foreign-slot-value interval-timespec '(:struct timespec) 'tv-sec) 0)
	(setf (cffi:foreign-slot-value interval-timespec '(:struct timespec) 'tv-nsec) 0)
	(setf (cffi:foreign-slot-value new-value '(:struct itimerspec) 'it-interval)
	      interval-timespec)

	(timerfd-settime timerfd 0 new-value (cffi:null-pointer))))
    (add-timer *loop* (make-instance 'timer-timer :fd timerfd :callback callback))))
