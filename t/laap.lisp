(in-package #:laap-test)

(def-suite laap :description "Baz")
(in-suite laap)

(test delay
  (laap:with-magic
    (format t "foo~%")
    (laap:delay 1)
    (format t "bar~%")
    (laap:delay 1)
    (format t "baz~%")))

(defun wait-and-print (i)
  (let ()
    (lambda ()
      (laap:deflaap %wait-and-print ()
	(format t "foo on thread ~a~%" (bt:thread-name (bt:current-thread)))
	(laap:delay (1+ (random 1.0)))
	(format t "bar on thread ~a~%" (bt:thread-name (bt:current-thread)))
	(laap:delay (1+ (random 1.0)))
	(format t "baz on thread ~a~%" (bt:thread-name (bt:current-thread))))
      (%wait-and-print))))

(test spawn-delay
  (laap:with-magic
    (loop for i below 3
       do (laap:spawn (wait-and-print i)))))
