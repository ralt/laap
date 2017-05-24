(in-package #:laap/test)

(defvar *tests* (make-hash-table))

(defmacro test (name var &body body)
  `(setf (gethash ',name *tests*) (lambda ,var ,@body)))

(defvar *results* nil)

(defun run-all-tests ()
  (maphash #'run *tests*))

(defun p (&rest args)
  (apply #'format t args)
  (force-output))

(defun run (test-name test-callback)
  (let ((result nil))
    (p "Testing ~a... " test-name)
    (laap:with-event-loop
      (laap:add-reporter (lambda (err)
			   (setf result err)))
      (funcall test-callback (lambda ())))
    (if result
	(p "failed with: ~a~%" test-name result)
	(p "passed.~%" test-name))))
