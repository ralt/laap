(in-package #:laap/fs)

;; open(2) flags
(defconstant +o-read-only+ 0)
(defconstant +o-write-only+ 1)
(defconstant +o-read-write+ 2)
(defconstant +o-create+ 64)

(defconstant +eintr+ 4)

(cffi:defcfun ("open" c-open) :int
  (pathname :string)
  (flags :int))

(cffi:defcfun ("close" c-close) :int
  (fd :int))

(cffi:defcvar ("errno" errno) :int)

(cffi:defcfun ("strerror" strerror) :string
  (errnum :int))

(cffi:defcfun ("read" c-read) :int
  (fd :int)
  (buf :pointer)
  (count :uint))

(cffi:defcfun ("write" c-write) :int
  (fd :int)
  (buf :pointer)
  (count :uint))

(cffi:defcfun ("rename" c-rename) :int
  (oldpath :string)
  (newpath :string))
