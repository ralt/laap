(defpackage #:laap/fs
  (:use #:cl)
  (:export #:file
	   #:read
	   #:write
	   #:close
	   #:rename
	   #:truncate
	   #:link
	   #:symlink)
  (:shadow #:read
	   #:write
	   #:close
	   #:truncate))
