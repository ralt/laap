(defpackage #:laap/socket
  (:use #:cl)
  (:export #:ipv4-socket
	   #:connect
	   #:close
	   #:send
	   #:receive
	   #:listen
	   #:accept)
  (:shadow #:close
	   #:listen))
