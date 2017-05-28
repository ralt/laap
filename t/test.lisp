(in-package #:laap/test)

(defvar *tests* (make-hash-table))

(defmacro test (name var &body body)
  `(progn
     (defun ,name ,var ,@body)
     (setf (gethash ',name *tests*) #',name)))

(defun run-all-tests ()
  (let ((end (bt:make-condition-variable))
	(end-lock (bt:make-lock))
	(counter (hash-table-count *tests*))
	(success t))
    (bt:with-lock-held (end-lock)
      (maphash (lambda (test-name test-callback)
		 (bt:make-thread
		  (lambda ()
		    (bt:with-lock-held (end-lock)
		      (run-test
		       test-name test-callback
		       (lambda (success-p)
			 (unless success-p
			   (setf success nil))
			 (decf counter)
			 (when (= counter 0)
			   (bt:condition-notify end))))))))
	       *tests*)
      (bt:condition-wait end end-lock))
    success))

(defun p (&rest args)
  (apply #'format t args)
  (force-output))

(defun run-test (test-name test-callback c)
  (flet ((done (&optional (err t))
	   (if (eq err t)
	       (p "passed.~%" test-name)
	       (p "failed with: ~a~%" test-name err))
	   (funcall c (eq err t))))
    (p "Testing ~a... " test-name)
    (laap:with-event-loop
      (laap:add-reporter #'done)
      (funcall test-callback #'done))))

(defun run (test-name)
  (flet ((done (&optional (err t))
	   (if (eq err t)
	       (p "passed.~%" test-name)
	       (progn
		 (p "failed with: ~a~%" test-name err)
		 (laap::%add-to-queue t)))))
    (p "Testing ~a... " test-name)
    (laap:with-event-loop
      (laap:add-reporter #'done)
      (funcall (gethash test-name *tests*) #'done))))
