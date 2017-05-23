(defpackage #:laap/fs
  (:use #:cl)
  (:export #:file
	   #:read
	   #:write
	   #:close
	   #:rename
	   #:truncate
	   #:link
	   #:symlink
	   #:unlink
	   #:readlink)
  (:shadow #:read
	   #:write
	   #:close
	   #:truncate))
