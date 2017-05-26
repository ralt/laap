(in-package #:laap/test)

(test socket-creation (done)
  (let ((socket (make-instance 'laap/socket:ipv4-socket)))
    (laap/socket:close socket
		       (lambda (err res)
			 (funcall done)))))

(test socket-connection (done)
  (let ((socket (make-instance 'laap/socket:ipv4-socket)))
    (laap/socket:connect
     socket
     (lambda (err res)
       (declare (ignore res))
       (when err (error err))
       (laap/socket:close socket
			  (lambda (err res)
			    (declare (ignore err res))
			    (funcall done))))
     :ip "127.0.0.1" :port 4242)))

(test socket-send-and-receive (done)
  (let ((socket (make-instance 'laap/socket:ipv4-socket)))
    (laap/socket:connect
     socket
     (lambda (err res)
       (when err (error err))
       (laap/socket:send
	socket
	(lambda (err result)
	  (when err (error err))
	  (let ((result ""))
	    (laap/socket:receive
	     socket
	     (lambda (err res)
	       (when err (error err))
	       (setf result (concatenate 'string result (babel:octets-to-string res))))
	     :end (lambda (err res)
		    (laap/socket:close socket (lambda (err res)
						(funcall done)))))))
	:data (babel:string-to-octets
	       (format nil "GET / HTTP/1.1~c~cHost: 127.0.0.1:4242~c~cConnection: close~c~c~c~c"
		       #\return #\linefeed
		       #\return #\linefeed
		       #\return #\linefeed
		       #\return #\linefeed))))
     :ip "127.0.0.1" :port 4242)))

(test socket-listen (done)
  (let ((socket (make-instance 'laap/socket:ipv4-socket)))
    (laap/socket:listen
     socket
     (lambda (err res)
       (assert (eq err nil))
       (laap/socket:close socket
			  (lambda (err res)
			    (funcall done))))
     :ip "127.0.0.1" :port 5557)))
