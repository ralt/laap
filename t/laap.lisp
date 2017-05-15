(in-package #:laap/test)

;;; Run with: ./buildapp --output bin/foo --eval '(declaim (optimize (speed 3)))' --asdf-tree ~/quicklisp/ --load-system laap-test --entry laap/test::main --eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c)))'
;;; for strace runs et al.
(defun main (&rest args)
  (declare (ignore args))
  (laap:with-event-loop
    (http-request (lambda (result)
		    (format t "~a" result)))))

(defun accept-loop (socket)
  (let ()
    (lambda (&optional err res)
      (when err (error err))
      (laap/socket:accept
       socket
       (accept-fn socket)))))

(defun accept-fn (socket)
  (let ()
    (lambda (err client-socket)
      (laap:spawn (accept-loop socket))
      (when err (error err))
      (laap/socket:send
       client-socket
       (lambda (err res)
	 (when err (error err))
	 (laap/socket:close
	  client-socket
	  #'laap:noop))
       :data (babel:string-to-octets (format nil
					     "HTTP/1.0 200 OK~c~cContent-Length: 6~c~c~c~cPong!~c~c~c"
					     #\return #\linefeed
					     #\return #\linefeed #\return #\linefeed
					     #\linefeed
					     #\return #\linefeed))))))

(defun http-request (done)
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
						(funcall done result)))))))
	:data (babel:string-to-octets
	       (format nil "GET / HTTP/1.1~c~cHost: 127.0.0.1:4242~c~cConnection: close~c~c~c~c"
		       #\return #\linefeed
		       #\return #\linefeed
		       #\return #\linefeed
		       #\return #\linefeed))))
     :ip "127.0.0.1" :port 80)))

(test delay (done)
  (format t "foo~%")
  (laap:delay 1
      (lambda (err res)
	(format t "bar~%")
	(laap:delay 1
	    (lambda (err res)
	      (format t "baz~%")
	      (funcall done))))))

(defun current-thread-name ()
  (bt:thread-name (bt:current-thread)))

(defun wait-and-print (done)
  (let ()
    (lambda (err res)
      (format t "foo on thread ~a~%" (current-thread-name))
      (laap:delay (1+ (random 1.0))
	  (lambda (err res)
	    (format t "bar on thread ~a~%" (current-thread-name))
	    (laap:delay (1+ (random 1.0))
		(lambda (err res)
		  (format t "baz on thread ~a~%" (current-thread-name))
		  (funcall done))))))))

(test spawn-delay (done)
  (loop for i below 3
     do (laap:spawn (wait-and-print done))))
