(defpackage #:laap/socket
  (:use #:cl)
  (:export #:ipv4-socket
	   #:connect
	   #:close)
  (:shadow #:close))
