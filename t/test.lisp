(in-package #:laap/test)

(defvar *tests* (make-hash-table))

(defmacro test (name var &body body)
  `(setf (gethash ',name *tests*) (lambda ,var ,@body)))

(defun run-all-tests ()
  (let ((end (bt:make-condition-variable))
	(end-lock (bt:make-lock))
	(counter (hash-table-count *tests*)))
    (maphash (lambda (test-name test-callback)
	       (bt:make-thread
		(lambda ()
		  (bt:with-lock-held (end-lock)
		    (run test-name test-callback
			 (lambda ()
			   (decf counter)
			   (when (= counter 0)
			     (bt:condition-notify end))))))))
	     *tests*)
    (bt:with-lock-held (end-lock)
      (bt:condition-wait end end-lock))))

(defun p (&rest args)
  (apply #'format t args)
  (force-output))

(defun run (test-name test-callback c)
  (flet ((done (&optional (err t))
	   (if (eq err t)
	       (p "passed.~%" test-name)
	       (p "failed with: ~a~%" test-name err))
	   (funcall c)))
    (p "Testing ~a... " test-name)
    (laap:with-event-loop
      (laap:add-reporter #'done)
      (funcall test-callback #'done))))
