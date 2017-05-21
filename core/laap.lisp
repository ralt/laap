(in-package #:laap)

(defvar *thread-pool* nil)
(defvar *loop* nil)

(defmacro with-event-loop (&body body)
  `(let* ((*thread-pool* (make-instance 'thread-pool))
	  (*loop* (make-instance 'event-loop))
	  (bt:*default-special-bindings* `((*thread-pool* . ,*thread-pool*)
					   (*loop* . ,*loop*))))
     #+sbcl
     (sb-sys:ignore-interrupt sb-unix:sigpipe)
     #-sbcl
     (warn "SIGPIPE can not be avoided in this implementation")
     (progn ,@body)
     (let ((thread-pool-thread (start-thread-pool)))
       (start-event-loops)
       (bt:join-thread thread-pool-thread)
       (maphash (lambda (thread props)
		  (declare (ignore props))
		  (bt:join-thread thread))
		(threads *thread-pool*)))))

(defun noop (err res)
  (declare (ignore err res)))
