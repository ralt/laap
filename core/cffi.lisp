(in-package #:laap)

;;; open/close
(defconstant +o-nonblock+ #o00004000)
(defconstant +o-cloexec+ 524288)

(cffi:defcfun ("write" c-write) :int
  (fd :int)
  (buf :pointer)
  (count :unsigned-int))

(cffi:defcfun ("close" c-close) :int
  (fd :int))

;;; errno
(defconstant +eperm+ 1)
(defconstant +enoent+ 2)
(defconstant +eintr+ 4)
(defconstant +ebadf+ 9)
(defconstant +enomem+ 12)
(defconstant +efault+ 14)
(defconstant +eexist+ 17)
(defconstant +einval+ 22)
(defconstant +enospc+ 28)
(defconstant +eloop+ 40)

(cffi:defcvar ("errno" errno) :int)

(cffi:defcfun ("strerror" strerror) :string
  (errnum :int))

;;; pipe
(cffi:defcfun ("pipe2" %pipe2) :int
  (pipefds :pointer)
  (flags :int))

(defun pipe ()
  (cffi:with-foreign-object (pipefds :int 2)
    (%pipe2 pipefds +o-nonblock+)
    (values (cffi:mem-aref pipefds :int 0) (cffi:mem-aref pipefds :int 1))))

;;; epoll
(cffi:defcfun ("epoll_create1" epoll-create1) :int
  (flags :int))

(defconstant +epoll-cloexec+ +o-cloexec+)

(defconstant +epoll-ctl-add+ 1)
(defconstant +epoll-ctl-del+ 2)
(defconstant +epoll-ctl-mod+ 3)

(defconstant +epollin+ 1)
(defconstant +epollpri+ 2)
(defconstant +epollout+ 4)
(defconstant +epollerr+ 8)
(defconstant +epollhup+ 16)

(defconstant +epolloneshot+ (ash 1 30))
(defconstant +epollet+ (ash 1 31))

(cffi:defcstruct (epoll-event :size 12)
  (events :uint32)
  (data :uint64))

(cffi:defcfun ("epoll_ctl" epoll-ctl) :int
  (epfd :int)
  (op :int)
  (fd :int)
  (event :pointer))

(cffi:defcfun ("epoll_wait" epoll-wait) :int
  (epfd :int)
  (events :pointer)
  (maxevents :int)
  (timeout :int))

;;; timers
(defconstant +clock-monotonic+ 1)

(defconstant +tfd-cloexec+ +o-cloexec+)

(cffi:defctype time-t :long)

(cffi:defcstruct timespec
  (tv-sec time-t)
  (tv-nsec :long))

(cffi:defcstruct itimerspec
  (it-interval (:struct timespec))
  (it-value (:struct timespec)))

(cffi:defcfun ("timerfd_create" timerfd-create) :int
  (clockid :int)
  (flags :int))

(cffi:defcfun ("timerfd_settime" timerfd-settime) :int
  (fd :int)
  (flags :int)
  (new-value :pointer)
  (old-value :pointer))

(cffi:defcfun ("timerfd_gettime" timerfd-gettime) :int
  (fd :int)
  (curr-value :pointer))
