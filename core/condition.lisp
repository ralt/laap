(in-package #:laap)

(define-condition os-error ()
  ((errno :initarg :errno :reader errno)))

(defmethod print-object ((condition os-error) stream)
  (format stream "~a" (strerror (errno condition))))

(deftype epoll-error-members ()
  `(member :err :hup :rdhup :pri))

(define-condition epoll-error ()
  ((error-type :initarg :error-type :reader error-type :type 'epoll-error-members)))
