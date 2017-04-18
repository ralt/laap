(in-package #:laap)

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
      (+ tv-sec (/ tv-usec 1000000)))))
