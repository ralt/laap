(in-package #:laap)

(defvar *error* nil
  "The error value shadowed by the handle-event and handle-error methods.")

(defvar *result* nil
  "The result value shadowed by the handle-error and handle-event methods.")

(defclass timer ()
  ((fd :initarg :fd :reader fd)
   (direction :initarg :direction :reader direction)
   (callback :initarg :callback :reader callback)
   (closed :initform nil :accessor closed)))

(defmethod handle-error ((timer timer) error)
  (let ((*error* error))
    (funcall (callback timer))))

(defgeneric handle-event (timer loop)
  (:documentation "Handles an event for a file descriptor"))

(defclass timer-timer (timer)
  ((direction :initform +epollin+)))

(defmethod handle-event ((timer timer-timer) loop)
  ;; We don't really need to read the timer fd, once we get
  ;; an event, it can only mean that it's ready.
  (unwind-protect
       (funcall (callback timer))
    (progn
      (setf (closed timer) t)
      (sb-ext:with-locked-hash-table ((timers loop))
	(remhash (fd timer) (timers loop)))
      (c-close (fd timer)))))
