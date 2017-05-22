(defpackage #:laap/fs
  (:use #:cl)
  (:export #:file
	   #:read
	   #:write
	   #:close
	   #:rename
	   #:truncate)
  (:shadow #:read
	   #:write
	   #:close
	   #:truncate))
