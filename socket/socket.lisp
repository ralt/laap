(in-package #:laap/socket)

(defclass socket ()
  ((fd :reader fd)
   (domain :reader socket-domain)
   (type :reader socket-type)
   (protocol :reader socket-protocol)))

(defclass ipv4-socket (socket) ())

(defmethod initialize-instance ((socket ipv4-socket) &key)
  (setf (slot-value socket 'domain) +af-inet+)
  (setf (slot-value socket 'type) (logior +sock-stream+ +sock-nonblock+))
  (setf (slot-value socket 'protocol) 0)
  (let ((socketfd (c-socket (socket-domain socket)
			    (socket-type socket)
			    (socket-protocol socket))))
    (when (= socketfd -1)
      (error (strerror errno)))
    (setf (slot-value socket 'fd) socketfd)))

(laap:defmethodpublic connect ((socket ipv4-socket) ip port callback)
  (let ((timer (make-instance 'timer-socket-connect :fd (fd socket) :callback callback)))
    (cffi:with-foreign-object (inp '(:struct in-addr))
      (cffi:with-foreign-string (cp ip)
	(inet-aton cp inp))
      (cffi:with-foreign-object (sockaddr '(:struct sockaddr-in))
	(cffi:with-foreign-slots ((sin-family sin-port sin-addr sin-zero)
				  sockaddr
				  (:struct sockaddr-in))
	  (setf sin-family (socket-domain socket))
	  (setf sin-port (htons port))
	  (setf sin-addr inp))
	(when (= (c-connect (fd socket) sockaddr (cffi:foreign-type-size '(:struct sockaddr-in))) -1)
	  ;; Non-blocking connect(2) always return EINPROGRESS.
	  ;; The fd needs to be added to the event loop and some
	  ;; dance with getsockopt(2) needs to be done to check
	  ;; the success or failure of the connect(2) call.
	  (unless (= errno +einprogress+)
	    (return-from %connect (laap:handle-error timer
						     (make-condition 'error (strerror errno)))))
	  (laap:add-timer laap:*loop* timer))))))

(defclass timer-socket-connect (laap:timer)
  ((laap:direction :initform +epollout+)))

(defmethod laap:handle-event ((timer timer-socket-connect) loop)
  (cffi:with-foreign-objects ((optval '(:pointer :int))
			      (optlen '(:pointer :uint)))
    (getsockopt (laap:fd timer) +sol-socket+ +so-error+ optval optlen)
    (when (= (cffi:mem-ref optval :int) 0)
      (funcall (laap:callback timer))
      (setf (laap:closed timer) t)
      (return-from laap:handle-event (laap:remove-timer loop timer)))
    (laap:handle-error timer (make-condition 'error (strerror (cffi:mem-ref optval :int))))))

(defun close (socket)
  (c-close (fd socket)))

(laap:defmethodpublic send ((socket ipv4-socket) data callback)
  (let ((timer (make-instance 'timer-socket-send
			      :fd (fd socket)
			      :callback callback
			      :data data)))
    ;; We don't need to wait for the socket to be ready,
    ;; we can write to it directly, and it will tell us
    ;; whenever we need to wait for it again.
    (laap:handle-event timer laap:*loop*)))

(defclass timer-socket-send (laap:timer)
  ((laap:direction :initform +epollout+)
   (data :initarg :data :accessor data)))

(defmethod laap:handle-event ((timer timer-socket-send) loop)
  (let ((flags +msg-nosignal+))
    (loop
       (let* ((data-length (length (data timer))))
	 (cffi:with-foreign-object (buf :char data-length)
	   (loop for i below data-length
	      do (setf (cffi:mem-ref buf :char i) (elt (data timer) i)))
	   (let ((sent (c-send (fd timer) buf data-length flags)))
	     ;; :(
	     (when (= sent -1)
	       (return-from laap:handle-event
		 (if (= errno +ewouldblock+)
		     (laap:add-timer loop timer)
		     (laap:handle-error timer (make-condition 'error
							      (strerror errno))))))
	     ;; :)
	     (when (= sent data-length)
	       (funcall (laap:callback timer))
	       (setf (laap:closed timer) t)
	       (return-from laap:handle-event (laap:remove-timer loop timer)))
	     ;; ¯\_(ツ)_/¯
	     (setf (data timer) (subseq (data timer) (1- sent)))))))))
