(in-package #:laap/socket)

;;; Domains
(defconstant +af-inet+ 2)

;;; Types
(defconstant +sock-stream+ 1)
(defconstant +sock-nonblock+ 2048)

;;; Errors
(defconstant +eagain+ 11)
(defconstant +ewouldblock+ 11)
(defconstant +einprogress+ 115)

;;; Epoll
(defconstant +epollin+ 1)
(defconstant +epollout+ 4)

;;; Socket options
(defconstant +sol-socket+ 1)
(defconstant +so-error+ 4)

;;; send(2) flags
(defconstant +msg-nosignal+ 16384)

;;; fcntl(2) flags
(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)

;;; open(2) flags
(defconstant +o-nonblock+ 2048)

(cffi:defcfun ("socket" c-socket) :int
  (domain :int)
  (type :int)
  (protocol :int))

(cffi:defcvar ("errno" errno) :int)

(cffi:defcfun ("strerror" strerror) :string
  (errnum :int))

(cffi:defcfun ("close" c-close) :int
  (fd :int))

(cffi:defcstruct in-addr
  (s-addr :uint32))

(cffi:defcstruct sockaddr-in
  (sin-family :short)
  (sin-port :unsigned-short)
  (sin-addr (:struct in-addr))
  (sin-zero :char :count 8))

(cffi:defcfun ("htons" htons) :uint16
  (hostshort :uint16))

(cffi:defcfun ("inet_aton" inet-aton) :int
  (cp (:pointer :char))
  (inp (:pointer (:struct in-addr))))

(cffi:defcfun ("connect" c-connect) :int
  (sockfd :int)
  (addr :pointer)
  (addrlen :uint))

(cffi:defcfun ("getsockopt" getsockopt) :int
  (sockfd :int)
  (level :int)
  (optname :int)
  (optval :pointer)
  (optlen :pointer))

(cffi:defcfun ("send" c-send) :int
  (sockfd :int)
  (buf :pointer)
  (len :unsigned-int)
  (flags :int))

(cffi:defcfun ("recv" c-recv) :int
  (sockfd :int)
  (buf :pointer)
  (len :unsigned-int)
  (flags :int))

(cffi:defcfun ("bind" c-bind) :int
  (sockfd :int)
  (addr :pointer)
  (addrlen :uint))

(cffi:defcfun ("listen" c-listen) :int
  (sockfd :int)
  (backlog :int))

(cffi:defcfun ("accept4" c-accept) :int
  (sockfd :int)
  (addr :pointer)
  (addrlen :pointer)
  (flags :int))

(cffi:defcfun ("fcntl" fcntl) :int
  (fd :int)
  (cmd :int)
  (flags :int))
