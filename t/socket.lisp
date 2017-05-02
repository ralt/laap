(in-package #:laap/test)

(def-suite laap/socket :description "Qux")
(in-suite laap/socket)

(test socket-creation
  (laap:with-magic
    (make-instance 'laap/socket:ipv4-socket)))

(test socket-connection
  (laap:with-magic
    (let ((socket (make-instance 'laap/socket:ipv4-socket)))
      (laap/socket:connect socket "8.8.8.8" 53)
      (laap/socket:close socket))))
