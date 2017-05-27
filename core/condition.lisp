(in-package #:laap)

(define-condition os-error ()
  ((errno :initarg :errno :reader errno)))

(defmethod print-object ((condition os-error) stream)
  (format stream "~a" (strerror (errno condition))))

(define-condition epoll-error ()
  ())
