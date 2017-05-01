(in-package #:laap/socket)

;;; Domains
(defconstant +af-unix+ 1)
(defconstant +af-local+ 1)
(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 10)
(defconstant +af-ipx+ 4)
(defconstant +af-netlink+ 16)
(defconstant +af-x25 9)
(defconstant +af-ax25+ 3)
(defconstant +af-atmpvc+ 8)
(defconstant +af-appletalk+ 5)
(defconstant +af-packet+ 17)
(defconstant +af-alg+ 38)

;;; Types
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)
(defconstant +sock-seqpacket+ 5)
(defconstant +sock-raw+ 3)
(defconstant +sock-rdm+ 4)
(defconstant +sock-packet+ 10)
(defconstant +sock-nonblock+ 2048)
(defconstant +sock-cloexec+ 524288)
(defconstant +sock-dccp+ 6)

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

(defgeneric connect (socket &key)
  (:documentation "Connect the socket"))

(defmethod connect ((socket ipv4-socket) &key ip port)
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
	))))
