(in-package #:laap)

;;; open/close
(defconstant +o-cloexec+ #o02000000)

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

;;; gettimeofday
(cffi:defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

(cffi:defcstruct timezone
  (tz-minuteswest :int)
  (tz-dsttime :int))

(defmethod translate-to-foreign (value (name (eql 'null-pointer)))
  (cond
    ((null value) (cffi:null-pointer))
    ((cffi:null-pointer-p value) value)
    (t (error "~A is not a null pointer." value))))

(cffi:defctype syscall-result :int)

(defmethod translate-from-foreign (value (name (eql 'syscall-result)))
  (if (minusp value)
      (error "System call failed with return value ~D." value)
      value))

(cffi:defcfun ("gettimeofday" %gettimeofday) syscall-result
  (tp :pointer)
  (tzp :pointer))

(defun gettimeofday ()
  (cffi:with-foreign-objects ((tv '(:pointer (:struct timeval))))
    (%gettimeofday tv (cffi:null-pointer))
    (cffi:with-foreign-slots ((tv-sec tv-usec) tv (:struct timeval))
      (values tv-sec tv-usec))))

;;; epoll
(cffi:defcfun ("epoll_create1" epoll-create1) :int
  (flags :int))

(defconstant +epoll-ctl-add+ 1)
(defconstant +epoll-ctl-del+ 2)
(defconstant +epoll-ctl-mod+ 3)

(defconstant +epollin+ 1)
(defconstant +epollout+ 4)
(defconstant +epollerr+ 8)
(defconstant +epollhup+ 16)

(defconstant +epolloneshot+ (ash 1 30))
(defconstant +epollet+ (ash 1 31))

(cffi:defcunion epoll-data
  (ptr :pointer)
  (fd :int)
  (u32 :uint32)
  (u64 :uint64))

(cffi:defctype epoll-data-t (:union epoll-data))

(cffi:defcstruct epoll-event
  (events :uint32)
  (data epoll-data-t))

(cffi:defcfun ("epoll_ctl" epoll-ctl) :int
  (epfd :int)
  (op :int)
  (fd :int)
  (event :pointer))

(cffi:defcstruct sigset-t
  (sig :unsigned-long))

(cffi:defcfun ("epoll_pwait" epoll-pwait) :int
  (epfd :int)
  (events :pointer)
  (maxevents :int)
  (timeout :int)
  (sigmask :pointer))

;;; timers
;; TODO: use CLOCK_MONOTONIC and replace gettimeofday with clock_gettime()
(defconstant +clock-realtime+ 0)

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
