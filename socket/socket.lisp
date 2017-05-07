(in-package #:laap/socket)

(defclass socket ()
  ((fd :reader fd)
   (domain :reader socket-domain)
   (type :reader socket-type)
   (protocol :reader socket-protocol)))

(defgeneric connect (socket callback &key)
  (:documentation "Connect the socket"))

(defgeneric close (socket callback &key)
  (:documentation "Close the socket"))

(defgeneric send (socket callback &key)
  (:documentation "Send data over a socket"))

(defgeneric receive (socket callback &key)
  (:documentation "Receive data from a socket"))

(defclass socket-timer (laap:timer) ())

(defmethod laap:handle-error ((timer socket-timer) error)
  (unwind-protect
       (funcall (laap:callback timer) error nil)
    (laap:remove-timer timer)))

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

(defmethod connect ((socket ipv4-socket) callback &key ip port)
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
	  (unless (= errno +einprogress+)
	    (return-from connect (laap:handle-error timer
						    (make-condition 'error (strerror errno)))))
	  (laap:add-timer timer))))))

(defclass timer-socket-connect (socket-timer)
  ((laap:direction :initform +epollout+)))

(defmethod laap:handle-event ((timer timer-socket-connect))
  (cffi:with-foreign-objects ((optval '(:pointer :int))
			      (optlen '(:pointer :uint)))
    (getsockopt (laap:fd timer) +sol-socket+ +so-error+ optval optlen)
    (when (= (cffi:mem-ref optval :int) 0)
      (unwind-protect
	   (funcall (laap:callback timer) nil nil)
	(return-from laap:handle-event (laap:remove-timer timer))))
    (laap:handle-error timer (make-condition 'error (strerror (cffi:mem-ref optval :int))))))

(defmethod close ((socket ipv4-socket) callback &key)
  (if (= (c-close (fd socket)) 0)
      (laap:spawn callback)
      (laap:handle-event (make-instance 'timer-socket-close :callback callback :errno errno))))

(defclass timer-socket-close (socket-timer)
  ((errno :initarg :errno :reader timer-errno)))

(defmethod laap:handle-event ((timer timer-socket-close))
  (unwind-protect
       (funcall (laap:callback timer)
		(make-condition 'error (strerror (timer-errno timer)))
		nil)
    (laap:remove-timer timer)))

(defmethod send ((socket ipv4-socket) callback &key data)
  (let ((timer (make-instance 'timer-socket-send
			      :fd (fd socket)
			      :callback callback
			      :data data)))
    ;; We don't need to wait for the socket to be ready,
    ;; we can write to it directly, and it will tell us
    ;; whenever we need to wait for it again.
    (laap:handle-event timer)))

(defclass timer-socket-send (socket-timer)
  ((laap:direction :initform +epollout+)
   (data :initarg :data :accessor data)))

(defmethod laap:handle-event ((timer timer-socket-send))
  (let ((flags +msg-nosignal+))
    (loop
       (let* ((data-length (length (data timer))))
	 (cffi:with-foreign-object (buf :char data-length)
	   (loop for i below data-length
	      do (setf (cffi:mem-aref buf :char i) (elt (data timer) i)))
	   (let ((sent (c-send (laap:fd timer) buf data-length flags)))
	     ;; :(
	     (when (= sent -1)
	       (return-from laap:handle-event
		 (if (= errno +ewouldblock+)
		     (laap:add-timer timer)
		     (laap:handle-error timer (make-condition 'error
							      (strerror errno))))))
	     ;; :)
	     (when (= sent data-length)
	       (unwind-protect
		    (funcall (laap:callback timer) nil nil)
		 (return-from laap:handle-event (laap:remove-timer timer))))
	     ;; ¯\_(ツ)_/¯
	     (setf (data timer) (subseq (data timer) (1- sent)))))))))

(defmethod receive ((socket ipv4-socket) callback &key end)
  (let ((timer (make-instance 'timer-socket-receive
			      :fd (fd socket)
			      :callback callback
			      :end-callback end)))
    ;; We don't need to wait for the socket to be ready,
    ;; we can read to it directly, and it will tell us
    ;; whenever we need to wait for it again.
    (laap:handle-event timer)))

(defclass timer-socket-receive (socket-timer)
  ((laap:direction :initform +epollin+)
   (end-callback :initarg :end-callback :reader end-callback)))

(defmethod laap:handle-event ((timer timer-socket-receive))
  (cffi:with-foreign-object (buf :char laap:*recv-buffer-length*)
    (let ((received (c-recv (laap:fd timer) buf laap:*recv-buffer-length* 0)))
      ;; :(
      (when (= received -1)
	(return-from laap:handle-event
	  (if (= errno +ewouldblock+)
	      (laap:add-timer timer)
	      (laap:handle-error timer (make-condition 'error
						       (strerror errno))))))
      ;; :)
      (let ((lisp-buffer (make-array received :element-type '(unsigned-byte 8))))
	(loop for i below received
	   do (setf (elt lisp-buffer i) (cffi:mem-aref buf :char i)))
	(funcall (if (= received 0)
		     (end-callback timer)
		     (laap:callback timer))
		 nil
		 lisp-buffer)
	(unless (= received 0)
	  (laap:handle-event timer))))))
