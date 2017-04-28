(in-package #:laap-test)

(def-suite laap :description "Baz")
(in-suite laap)

(defun main (&rest args)
  (laap:with-magic
    (loop for i from 0 upto 10
       do (laap:spawn 'wait-and-print))))

(laap:deflaap wait-and-print ()
  (format t "foo~%")
  (laap:delay 2)
  (format t "bar~%")
  (laap:delay 2)
  (format t "baz~%"))

(test delay
  (laap:with-magic
    (loop for i from 0 upto 10
       do (laap:spawn 'wait-and-print))))
