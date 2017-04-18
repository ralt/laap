(in-package #:laap-test)

(def-suite laap :description "Baz")
(in-suite laap)

(test delay
  (laap:with-magic
    (loop for i from 1 upto 3
       do (laap:delay i
		      (lambda (i)
			(format t "~a~%" i))
		      i))))
