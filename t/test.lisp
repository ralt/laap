(in-package #:laap/test)

(defvar *tests* (make-hash-table))

(defmacro test (name var &body body)
  `(setf (gethash ',name *tests*) (lambda ,var ,@body)))

(defvar *results* nil)

(defun run-all-tests ()
  (maphash #'run *tests*))

(defun run (test-name test-callback)
  (let ((result nil))
    (laap:with-event-loop
      (laap:add-reporter (lambda (err)
			   (setf result err)))
      (funcall test-callback (lambda ())))
    (if result
	(format t "~a failed with: ~a~%" test-name result)
	(format t "~a passed.~%" test-name))))
