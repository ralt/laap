(in-package #:laap)

(defvar *thread-should-exit* nil)

(defclass thread-pool ()
  ((event :accessor event)
   (queue :accessor queue :initform nil)
   (lock :accessor lock)
   (threads :accessor threads)
   (max-event-loops :accessor max-event-loops)
   (action-queue :accessor action-queue)
   (action-queue-lock :accessor action-queue-lock)
   (reporters :accessor reporters)
   (reporters-lock :accessor reporters-lock)))

(defmethod initialize-instance ((pool thread-pool) &key)
  (setf (queue pool) nil)
  (setf (lock pool) (bt:make-recursive-lock))
  (setf (event pool) (bt:make-condition-variable))
  (setf (threads pool) (make-hash-table :test 'eq))
  (setf (action-queue pool) (make-hash-table :test 'eq))
  (setf (action-queue-lock pool) (bt:make-recursive-lock))
  (setf (reporters pool) nil)
  (setf (reporters-lock pool) (bt:make-recursive-lock)))

(defclass action () ())

(defgeneric execute (action)
  (:documentation "Execute an action."))

(defun start-thread-pool ()
  (bt:make-thread
   (lambda ()
     (bt:with-recursive-lock-held ((lock *thread-pool*))
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
  (bt:with-recursive-lock-held ((lock *thread-pool*))
    (bt:with-recursive-lock-held ((action-queue-lock *thread-pool*))
      (setf (gethash action (action-queue *thread-pool*)) (make-instance 'action-queue-item)))
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
  (let ((action-item (bt:with-recursive-lock-held ((action-queue-lock *thread-pool*))
		       (gethash action (action-queue *thread-pool*)))))
    (unwind-protect
	 (bt:with-recursive-lock-held ((lock action-item))
	   (loop
	      (progn
		(when (slot-boundp action-item 'result)
		  (return-from wait-from-action-queue
		    (result action-item)))
		(bt:condition-wait (event action-item) (lock action-item)))))
      (bt:with-recursive-lock-held ((action-queue-lock *thread-pool*))
	(remhash action (action-queue *thread-pool*))))))

(defun add-to-action-queue (action value)
  (let ((action-item (bt:with-recursive-lock-held ((action-queue-lock *thread-pool*))
		       (gethash action (action-queue *thread-pool*)))))
    (bt:with-recursive-lock-held ((lock action-item))
      (setf (result action-item) value))
    (bt:condition-notify (event action-item))))

(defclass thread-properties ()
  ((blocking :initarg :blocking :accessor blocking)))

(defclass new-thread-action (action)
  ((callback :initarg :callback :reader callback)))

(defmethod execute ((action new-thread-action))
  (let ((bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					 (*loop* . ,*loop*)
					 (*thread-should-exit* . nil))))
    (setf (gethash (bt:make-thread
		    (lambda ()
		      (handler-case
			  (progn
			    (funcall (callback action)))
			(error (e)
			  (add-to-queue (make-instance
					 'thread-error
					 :error e
					 :thread (bt:current-thread)))))))
		   (threads *thread-pool*))
	  (make-instance 'thread-properties :blocking 0))))

(defun event-loop-action ()
  (make-instance 'new-thread-action
		 :callback (lambda ()
			     (main-loop (efd *loop*)))))

(defun add-reporter (reporter)
  (push reporter (reporters *thread-pool*)))

(defclass thread-error (action)
  ((thread :initarg :thread :reader thread)
   (err :initarg :error :reader err)))

(defmethod execute ((action thread-error))
  (remhash (thread action) (threads *thread-pool*))
  (bt:with-recursive-lock-held ((reporters-lock *thread-pool*))
    (dolist (reporter (reporters *thread-pool*))
      (funcall reporter (err action))))
  (add-to-queue (event-loop-action)))

(defun quit-event-loop ()
  (add-to-queue t)
  t)

(defclass blocking-action (action)
  ((thread :initarg :thread :reader thread)))

(defmethod execute ((action blocking-action))
  (incf (blocking (gethash (thread action) (threads *thread-pool*))))
  (let ((event-loop-threads 0))
    (maphash (lambda (thread props)
	       (declare (ignore thread))
	       (when (= (blocking props) 0)
		 (incf event-loop-threads)))
	     (threads *thread-pool*))
    (when (= event-loop-threads 0)
      (execute (event-loop-action)))))

(defclass unblocking-action (action)
  ((thread :initarg :thread :reader thread)))

(defmethod execute ((action unblocking-action))
  (decf (blocking (gethash (thread action) (threads *thread-pool*))))
  (let ((event-loop-threads 0))
    (maphash (lambda (thread props)
	       (declare (ignore thread))
	       (when (= (blocking props) 0)
		 (incf event-loop-threads)))
	     (threads *thread-pool*))
    (if (and (= (blocking (gethash (thread action) (threads *thread-pool*))) 0)
	     (> event-loop-threads (max-event-loops *thread-pool*)))
	(progn
	  (remhash (thread action) (threads *thread-pool*))
	  (add-to-action-queue action t))
	(add-to-action-queue action nil))))

(defmacro with-blocking-thread (name &body body)
  (let ((current-thread (gensym))
	(action (gensym))
	(should-exit (gensym)))
    `(let ((,current-thread (bt:current-thread)))
       (add-to-queue (make-instance 'blocking-action :thread ,current-thread))
       (unwind-protect
	    (funcall (lambda () (block ,name ,@body)))
	 (let ((,action (make-instance 'unblocking-action :thread ,current-thread)))
	   (add-to-queue ,action)
	   (let ((,should-exit (wait-from-action-queue ,action)))
	     (when ,should-exit
	       (setf *thread-should-exit* t))))))))
