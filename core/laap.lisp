(in-package #:laap)

(defvar *loop* (make-instance 'event-loop))

(defun start ()
  (start-loop *loop*))

(defun noop (err res)
  (declare (ignore err res)))
