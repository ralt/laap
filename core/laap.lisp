(in-package #:laap)

(defvar *thread-pool* nil)
(defvar *loop* nil)

(defmacro with-event-loop (&body body)
  `(let* ((*thread-pool* (make-instance 'thread-pool))
	  (*loop* (make-instance 'event-loop))
	  (bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					   (*loop* . ,*loop*))))
     (progn ,@body)
     (let ((thread-pool-thread (start-thread-pool)))
       (start-event-loop)
       (bt:join-thread thread-pool-thread)
       (dolist (thread (event-loop-threads *thread-pool*))
	 (bt:join-thread thread)))))

(defun noop (err res)
  (declare (ignore err res)))
