(in-package #:laap-test)

(def-suite laap :description "Baz")
(in-suite laap)

(test delay
  (laap:with-magic
    (format t "foo~%")
    (laap:delay 2)
    (format t "bar~%")
    (laap:delay 2)
    (format t "baz~%")))
