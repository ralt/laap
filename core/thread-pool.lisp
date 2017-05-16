(in-package #:laap)

(defclass thread-pool ()
  ((event :accessor event)
   (queue :accessor queue :initform nil)
   (queue-lock :accessor queue-lock)
   (event-loop-threads :accessor event-loop-threads)
   (blocking-threads :accessor blocking-threads)))

(defmethod initialize-instance ((pool thread-pool) &key)
  (setf (queue pool) nil)
  (setf (queue-lock pool) (bt:make-lock))
  (setf (event pool) (bt:make-condition-variable))
  (setf (event-loop-threads pool) nil)
  (setf (blocking-threads pool) nil))

(defclass action ()
  ((callback :initarg :callback :reader callback)))

(defgeneric execute (action)
  (:documentation "Execute an action."))

(defun start-thread-pool ()
  (bt:make-thread
   (lambda ()
     (bt:with-lock-held ((queue-lock *thread-pool*))
       (block loop
	 (loop
	    (loop for action = (pop (queue *thread-pool*))
	       until (eq action nil)
	       when (eq action t)
	       do (return-from loop (progn
				      (dolist (thread (event-loop-threads *thread-pool*))
					(bt:interrupt-thread thread (lambda ())))
				      (c-close (efd *loop*))))
	       do (execute action))
	    (bt:condition-wait (event *thread-pool*) (queue-lock *thread-pool*))))))))

(defun add-to-pool (action)
  (bt:with-lock-held ((queue-lock *thread-pool*))
    (setf (queue *thread-pool*) (append (queue *thread-pool*) (list action))))
  (bt:condition-notify (event *thread-pool*)))

(defun add-event-loop-thread (fn)
  (add-to-pool (make-instance 'event-loop-action :callback fn)))

(defun execute-thread (threads-list name callback)
  (let ((bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					 (*loop* . ,*loop*))))
    (push (bt:make-thread
	   callback
	   :name name)
	  threads-list)))

(defclass event-loop-action (action) ())

(defmethod execute ((action event-loop-action))
  (execute-thread (event-loop-threads *thread-pool*)
		  "Event loop thread"
		  (callback action)))

(defun quit-event-loop ()
  (add-to-pool t))

(defclass blocking-action (action) ())

(defmethod execute ((action blocking-action))
  (execute-thread (blocking-threads *thread-pool*)
		  "Blocking thread"
		  (callback action)))

(defmacro with-blocking-thread (name &body body)
  `(add-to-pool (make-instance 'blocking-action
			       :callback (lambda ()
					   (block ,name
					     ,@body)))))
