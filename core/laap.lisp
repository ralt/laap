(in-package #:laap)

(defvar *thread-pool* nil)
(defvar *loop* nil)

(defmacro with-event-loop (&body body)
  (let ((err (gensym))
	(res (gensym)))
    `(let* ((*thread-pool* (make-instance 'thread-pool))
	    (*loop* (make-instance 'event-loop))
	    (bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					     (*loop* . ,*loop*))))
       (add-reporter (lambda (err)
		       (format t "A thread died with error: ~a~%" err)))
       (unwind-protect
	    ;; We're immediately adding to the event loop,
	    ;; so that if it starts with a blocking thread,
	    ;; it will first go through the event loop, making
	    ;; sure both the thread pool and the event loop
	    ;; threads are started.
	    (spawn (lambda (,err ,res)
		     (declare (ignore ,err ,res))
		     ,@body))
	 (let ((thread-pool-thread (start-thread-pool)))
	   (start-event-loops)
	   (bt:join-thread thread-pool-thread)
	   (maphash (lambda (thread props)
		      (declare (ignore props))
		      (bt:join-thread thread))
		    (threads *thread-pool*)))))))

(defun noop (err res)
  (declare (ignore err res)))
