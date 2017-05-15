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

(defclass action () ())

(defgeneric execute (action)
  (:documentation "Execute an action."))

(defun start-thread-pool ()
  (bt:make-thread
   (lambda ()
     (bt:with-lock-held ((queue-lock *thread-pool*))
       (loop
	  (loop for action = (pop (queue *thread-pool*))
	     until (eq action nil)
	     when (eq action t)
	     do (return-from start-thread-pool (loop for thread in (event-loop-threads *thread-pool*)
						  do (bt:interrupt-thread thread (lambda ()))))
	     do (execute action))
	  (bt:condition-wait (event *thread-pool*) (queue-lock *thread-pool*)))))))

(defun add-to-pool (action)
  (bt:with-lock-held ((queue-lock *thread-pool*))
    (setf (queue *thread-pool*) (append (queue *thread-pool*) (list action))))
  (bt:condition-notify (event *thread-pool*)))

(defun add-event-loop-thread (fn)
  (add-to-pool (make-instance 'add-event-loop-action :callback fn)))

(defclass add-event-loop-action (action)
  ((callback :initarg :callback :reader callback)))

(defmethod execute ((action add-event-loop-action))
  (let ((bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					 (*loop* . ,*loop*))))
    (push (bt:make-thread
	   (callback action)
	   :name "Event loop thread")
	  (event-loop-threads *thread-pool*))))

(defun quit-event-loop ()
  (add-to-pool t))
