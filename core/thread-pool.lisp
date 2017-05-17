(in-package #:laap)

(defclass thread-pool ()
  ((event :accessor event)
   (queue :accessor queue :initform nil)
   (lock :accessor lock)
   (threads :accessor threads)
   (max-event-loops :accessor max-event-loops)
   (action-queue :accessor action-queue)))

(defmethod initialize-instance ((pool thread-pool) &key)
  (setf (queue pool) nil)
  (setf (lock pool) (bt:make-lock))
  (setf (event pool) (bt:make-condition-variable))
  (setf (threads pool) (make-hash-table :test 'eq))
  (setf (action-queue pool) (make-hash-table :test 'eq)))

(defclass action () ())

(defgeneric execute (action)
  (:documentation "Execute an action."))

(defun start-thread-pool ()
  (bt:make-thread
   (lambda ()
     (bt:with-lock-held ((lock *thread-pool*))
       (block loop
	 (loop
	    (loop for action = (pop (queue *thread-pool*))
	       until (eq action nil)
	       when (eq action t)
	       do (return-from loop (progn
				      (maphash (lambda (thread properties)
						 (declare (ignore properties))
						 (bt:interrupt-thread thread (lambda ())))
					       (threads *thread-pool*))
				      (c-close (efd *loop*))))
	       do (execute action))
	    (bt:condition-wait (event *thread-pool*) (lock *thread-pool*))))))))

(defun add-to-queue (action)
  (bt:with-lock-held ((lock *thread-pool*))
    (setf (gethash action (action-queue *thread-pool*)) (make-instance 'action-queue-item))
    (setf (queue *thread-pool*) (append (queue *thread-pool*) (list action))))
  (bt:condition-notify (event *thread-pool*)))

(defun add-thread (fn)
  (add-to-queue (make-instance 'new-thread-action
			       :callback fn)))

(defclass action-queue-item ()
  ((lock :accessor lock)
   (event :accessor event)
   (result :accessor result)))

(defmethod initialize-instance ((item action-queue-item) &key)
  (setf (lock item) (bt:make-lock))
  (setf (event item) (bt:make-condition-variable)))

(defun wait-from-action-queue (action)
  (let ((action-item (gethash action (action-queue *thread-pool*))))
    (bt:with-lock-held ((lock action-item))
      (loop
	 (progn
	   (when (slot-boundp action-item 'result)
	     (return-from wait-from-action-queue
	       (progn
		 (remhash action (action-queue *thread-pool*))
		 (result action-item))))
	   (bt:condition-wait (event action-item) (lock action-item)))))))

(defun add-to-action-queue (action value)
  (let ((action-item (gethash action (action-queue *thread-pool*))))
    (bt:with-lock-held ((lock action-item))
      (setf (result action-item) value))
    (bt:condition-notify (event action-item))))

(defclass thread-properties ()
  ((blocking :initarg :blocking :accessor blocking)))

(defclass new-thread-action (action)
  ((callback :initarg :callback :reader callback)))

(defmethod execute ((action new-thread-action))
  (let ((bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					 (*loop* . ,*loop*))))
    (setf (gethash (bt:make-thread
		    (callback action))
		   (threads *thread-pool*))
	  (make-instance 'thread-properties :blocking 0))))

(defun quit-event-loop ()
  (add-to-queue t))

(defclass blocking-action (action)
  ((thread :initarg :thread :reader thread)))

(defmethod execute ((action blocking-action))
  (incf (blocking (gethash (thread action) (threads *thread-pool*))))
  (let ((event-loop-threads 0))
    (maphash (lambda (thread props)
	       (declare (ignore thread))
	       (when (> (blocking props) 0)
		 (incf event-loop-threads)))
	     (threads *thread-pool*))
    (when (= event-loop-threads 0)
      (execute (make-instance 'new-thread-action :callback (lambda ()
							     (main-loop (efd *loop*))))))))

(defclass unblocking-action (action)
  ((thread :initarg :thread :reader thread)))

(defmethod execute ((action unblocking-action))
  (let ((event-loop-threads 0))
    (maphash (lambda (thread props)
	       (declare (ignore thread))
	       (when (> (blocking props) 0)
		 (incf event-loop-threads)))
	     (threads *thread-pool*))
    (decf (blocking (gethash (thread action) (threads *thread-pool*))))
    (add-to-action-queue action (= event-loop-threads (max-event-loops *thread-pool*)))))

(defmacro with-blocking-thread (name &body body)
  (let ((callback (gensym))
	(action (gensym))
	(should-exit (gensym))
	(current-thread (gensym)))
    `(let ((,callback (lambda () (block ,name ,@body)))
	   (,current-thread (bt:current-thread))
	   (,action (make-instance 'blocking-action :thread ,current-thread)))
       (add-to-queue ,action)
       (unwind-protect
	    (funcall ,callback)
	 (let ((,action (make-instance 'unblocking-action :thread ,current-thread)))
	   (add-to-queue ,action)
	   (let ((,should-exit (wait-from-action-queue ,action)))
	     (when ,should-exit
	       (pthread-exit (cffi:null-pointer)))))))))
