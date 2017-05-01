(in-package #:laap/socket)

(cffi:defcfun ("socket" c-socket) :int
  (domain :int)
  (type :int)
  (protocol :int))

(cffi:defcvar ("errno" errno) :int)

(cffi:defcfun ("strerror" strerror) :string
  (errnum :int))

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
