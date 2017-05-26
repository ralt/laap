(in-package #:laap/socket)

(defclass socket ()
  ((fd :initarg :fd :reader fd)
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

(defgeneric listen (socket callback &key)
  (:documentation "Listen on a socket"))

(defgeneric accept (socket callback &key)
  (:documentation "Accept a connection from a socket"))

(defclass socket-timer (laap:timer) ())

(defmethod laap:handle-error ((timer socket-timer) error)
  (unwind-protect
       (funcall (laap:callback timer) error nil)
    (laap:remove-timer timer)))

(defclass ipv4-socket (socket) ())

(defmethod initialize-instance :after ((socket ipv4-socket) &key)
  (setf (slot-value socket 'domain) +af-inet+)
  (setf (slot-value socket 'type) (logior +sock-stream+ +sock-nonblock+))
  (setf (slot-value socket 'protocol) 0)
  (if (slot-boundp socket 'fd)
      (let ((flags (fcntl (fd socket) +f-getfl+ 0)))
	(fcntl (fd socket) +f-setfl+ (logior flags +o-nonblock+)))
      (let ((socketfd (c-socket (socket-domain socket)
				(socket-type socket)
				(socket-protocol socket))))
	(when (= socketfd -1)
	  (error (make-condition 'laap:os-error :errno errno)))
	(setf (slot-value socket 'fd) socketfd))))

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
						    (make-condition 'laap:os-error :errno errno)))))
	(laap:add-timer timer)))))

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
    (laap:handle-error timer (make-condition 'laap:os-error :errno (cffi:mem-ref optval :int)))))

(defmethod close ((socket ipv4-socket) callback &key)
  (laap:spawn (lambda (err res)
		(declare (ignore err res))
		(if (= (c-close (fd socket)) 0)
		    (funcall callback nil nil)
		    (funcall callback (make-condition 'laap:os-error :errno errno) nil)))))

(defmethod send ((socket ipv4-socket) callback &key data)
  (laap:add-timer (make-instance 'timer-socket-send
				 :fd (fd socket)
				 :callback callback
				 :data data)))

(defclass timer-socket-send (socket-timer)
  ((laap:direction :initform +epollout+)
   (data :initarg :data :accessor data)))

(defmethod laap:handle-event ((timer timer-socket-send))
  (loop
     (let* ((data-length (length (data timer))))
       (cffi:with-foreign-object (buf :char data-length)
	 (loop for i below data-length
	    do (setf (cffi:mem-aref buf :char i) (elt (data timer) i)))
	 (let ((sent (c-send (laap:fd timer) buf data-length 0)))
	   ;; :(
	   (when (= sent -1)
	     (return-from laap:handle-event
	       (if (= errno +ewouldblock+)
		   (laap:add-timer timer)
		   (laap:handle-error timer (make-condition 'laap:os-error :errno errno)))))
	   ;; :)
	   (when (= sent data-length)
	     (unwind-protect
		  (funcall (laap:callback timer) nil nil)
	       (return-from laap:handle-event (laap:remove-timer timer))))
	   ;; ¯\_(ツ)_/¯
	   (setf (data timer) (subseq (data timer) (1- sent))))))))

(defmethod receive ((socket ipv4-socket) callback &key end)
  (laap:add-timer (make-instance 'timer-socket-receive
				 :fd (fd socket)
				 :callback callback
				 :end-callback end)))

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
	      (laap:handle-error timer (make-condition 'laap:os-error :errno errno)))))
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
	  (unwind-protect
	       (laap:handle-event timer)
	    (laap:remove-timer timer)))))))

(defmethod listen ((socket ipv4-socket) callback &key ip port (backlog 768))
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
      (when (= (c-bind (fd socket) sockaddr (cffi:foreign-type-size '(:struct sockaddr-in))) -1)
	(return-from listen (funcall callback (make-condition 'laap:os-error :errno errno) nil)))
      (when (= (c-listen (fd socket) backlog) -1)
	(return-from listen (funcall callback (make-condition 'laap:os-error :errno errno) nil)))
      (funcall callback nil nil))))

(defmethod accept ((socket ipv4-socket) callback &key)
  (laap:add-timer (make-instance 'timer-socket-accept
				 :fd (fd socket)
				 :callback callback)))

(defclass timer-socket-accept (socket-timer)
  ((laap:direction :initform +epollin+)))

(defmethod laap:handle-event ((timer timer-socket-accept))
  (let ((accepted-sockfd (c-accept (laap:fd timer) (cffi:null-pointer) (cffi:null-pointer) 0)))
    (if (= accepted-sockfd -1)
	(if (= errno +eagain+)
	    (laap:add-timer timer)
	    (unwind-protect
		 (laap:handle-error timer (make-condition 'laap:os-error :errno errno))
	      (laap:remove-timer timer)))
	(unwind-protect
	     (funcall (laap:callback timer) nil (make-instance 'ipv4-socket
							       :fd accepted-sockfd))
	  (laap:remove-timer timer)))))
