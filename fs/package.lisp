(defpackage #:laap/fs
  (:use #:cl)
  (:export #:file
	   #:read
	   #:write
	   #:close
	   #:rename
	   #:truncate
	   #:link)
  (:shadow #:read
	   #:write
	   #:close
	   #:truncate))
