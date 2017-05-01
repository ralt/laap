(in-package #:laap/test)

(def-suite laap/socket :description "Qux")
(in-suite laap/socket)

(test socket-creation
  (laap:with-magic
    (let ((socket (make-instance 'laap/socket:ipv4-socket)))
      (laap/socket:connect socket :ip "8.8.8.8" :port 53))))
