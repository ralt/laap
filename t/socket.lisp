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
      (laap/socket:send socket (babel:string-to-octets
				(format nil "GET / HTTP/1.1~%~%")))
      (laap/socket:close socket))))

#| Don't break CI yet.
(test socket-send-and-receive
  (laap:with-magic
    (let ((socket (make-instance 'laap/socket:ipv4-socket)))
      (laap/socket:connect socket "127.0.0.1" 4242)
      (laap/socket:send socket (babel:string-to-octets
				(format nil "GET / HTTP/1.1~%~%")))
      (let ((result ""))
	(format t "result: ~a~%" result)
	(block receive
	  (loop
	     (progn
	       (format t "loop~%")
	       ;(laap/socket:receive socket)
	       (format t "laap~%")
	       (let ((data (laap/socket:receive socket)))
		 (when (= (length data) 0)
		   (return-from receive result))
		 (setf result (concatenate 'string
					   result
					   (babel:octets-to-string data)))))))
	(format t "result: ~a~%" result))
      (laap/socket:close socket))))
|#
