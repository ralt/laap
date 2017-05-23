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

(cffi:defcfun ("ftruncate" c-ftruncate) :int
  (fd :int)
  (length :long))

(cffi:defcfun ("link" c-link) :int
  (oldpath :string)
  (newpath :string))

(cffi:defcfun ("symlink" c-symlink) :int
  (target :string)
  (newpath :string))

(cffi:defcfun ("unlink" c-unlink) :int
  (pathname :string))

(cffi:defcstruct stat
  (mode-t :uint)
  (st-ino :ulong)
  (st-dev :uint32)
  (st-nlink :uint32)
  (st-uid :uint)
  (st-gid :uint)
  (st-size :long)
  (st-atime :long)
  (st-mtime :long)
  (st-ctime :long))

(cffi:defcfun ("lstat" c-lstat) :int
  (pathname :string)
  (stat :pointer))

(defconstant +path-max+ 4096)

(cffi:defcfun ("readlink" c-readlink) :int
  (pathname :string)
  (buf :string)
  (bufsiz :uint))
