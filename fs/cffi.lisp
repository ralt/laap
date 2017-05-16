(in-package #:laap/fs)

(defconstant +eintr+ 4)

(cffi:defcfun ("open" c-open) :int
  (pathname :string)
  (flags :int))

(cffi:defcvar ("errno" errno) :int)

(cffi:defcfun ("strerror" strerror) :string
  (errnum :int))

(cffi:defcfun ("read" c-read) :int
  (fd :int)
  (buf :pointer)
  (count :uint))
