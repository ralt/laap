(in-package #:laap/test)

(defvar *tests* (make-hash-table))

(defmacro test (name var &body body)
  `(setf (gethash ',name *tests*) (lambda ,var ,@body)))

(defvar *results* (make-hash-table))

(defun run-all-tests ()
  (let ((*results* (make-hash-table)))
    (maphash (lambda (k v)
	       (handler-case
		   (progn
		     (funcall v (lambda ()))
		     (setf (gethash k *results*) t))
		 (error (e)
		   (setf (gethash k *results*) e))))
	     *tests*)
    (laap:start)
    (check-results)))

(defun run (test)
  (let ((*results* (make-hash-table))
	(test-callback (gethash test *tests*)))
    (handler-case
	(progn
	  (funcall test-callback (lambda ()))
	  (setf (gethash test *results*) t))
      (error (e)
	(setf (gethash test *results*) e)))
    (laap:start)
    (check-results)))

(defun check-results ()
  (maphash
   (lambda (test-name result)
     (if result
	 (format t "~a passed.~%" test-name)
	 (progn
	   (format *error-output* "~a failed with: ~a~%" test-name result)
	   (return-from check-results))))
   *results*)
  t)
