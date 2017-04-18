(in-package #:laap)

(defvar *loop* nil)

(defmacro with-event-loop (&body body)
  `(progn
     (setf *loop* (make-instance 'event-loop))
     (progn
       (progn ,@body)
       (start *loop*))))

(defmacro with-magic (&body body)
  `(with-event-loop
     ,@body))

(defun delay (seconds callback &rest args)
  (add-callback *loop* (+ seconds (gettimeofday)) callback args))
