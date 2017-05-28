(in-package #:laap)

(defvar *thread-pool* nil)
(defvar *loop* nil)

(defmacro with-event-loop (&body body)
  "Run code in the laap event loop."
  (let ((err (gensym))
	(res (gensym)))
    `(let* ((*thread-pool* (make-instance 'thread-pool))
	    (*loop* (make-instance 'event-loop))
	    (bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					     (*loop* . ,*loop*))))
       ;; We're immediately adding to the event loop,
       ;; so that if it starts with a blocking thread,
       ;; it will first go through the event loop, making
       ;; sure both the thread pool and the event loop
       ;; threads are started.
       (spawn (lambda (,err ,res)
		(declare (ignore ,err ,res))
		,@body))
       (bt:join-thread (start-thread-pool)))))

(defun noop (err res)
  (declare (ignore err res)))
