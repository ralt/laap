(defpackage #:laap/fs
  (:use #:cl)
  (:export #:open
	   #:file
	   #:read)
  (:shadow #:open
	   #:read))
