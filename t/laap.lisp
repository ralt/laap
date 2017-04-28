(in-package #:laap-test)

(def-suite laap :description "Baz")
(in-suite laap)

(defun main (&rest args)
  (laap:with-magic
    (loop for i from 0 upto 10
       do (laap:spawn 'wait-and-print))))

(defun wait-and-print (i)
  (let ()
    (lambda ()
      (let ((laap::self nil))
	(cl-coroutine:defcoroutine foo ()
	  (format t "foo ~a~%" i)
	  (laap:delay 2)
	  (format t "bar ~a~%" i)
	  (laap:delay 2)
	  (format t "baz ~a~%" i))
	(setf laap::self (cl-coroutine:make-coroutine 'foo))
	(funcall laap::self)))))

(test delay
  (laap:with-magic
    (loop for i from 0 upto 10
       do (laap:spawn (wait-and-print i)))
    (laap:delay 5)))
