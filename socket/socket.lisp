(in-package #:laap/socket)

(defmacro define-exported-constant (constant value)
  `(progn
     (defconstant ,constant ,value)
     (export ',constant)))

;;; Domains
(define-exported-constant +af-unix+ 1)
(define-exported-constant +af-local+ 1)
(define-exported-constant +af-inet+ 2)
(define-exported-constant +af-inet6+ 10)
(define-exported-constant +af-ipx+ 4)
(define-exported-constant +af-netlink+ 16)
(define-exported-constant +af-x25 9)
(define-exported-constant +af-ax25+ 3)
(define-exported-constant +af-atmpvc+ 8)
(define-exported-constant +af-appletalk+ 5)
(define-exported-constant +af-packet+ 17)
(define-exported-constant +af-alg+ 38)

;;; Types
(define-exported-constant +sock-stream+ 1)
(define-exported-constant +sock-dgram+ 2)
(define-exported-constant +sock-seqpacket+ 5)
(define-exported-constant +sock-raw+ 3)
(define-exported-constant +sock-rdm+ 4)
(define-exported-constant +sock-packet+ 10)
(define-exported-constant +sock-nonblock+ #x004000)
(define-exported-constant +sock-cloexec+ #x400000)
(define-exported-constant +sock-dccp+ 6)

(defun socket (domain type &optional (protocol 0))
  )
