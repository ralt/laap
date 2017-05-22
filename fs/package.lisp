(defpackage #:laap/fs
  (:use #:cl)
  (:export #:open
	   #:file
	   #:read
	   #:write
	   #:close
	   #:rename

	   #:+o-read-only+
	   #:+o-write-only+
	   #:+o-read-write+

	   #:+o-create+)
  (:shadow #:open
	   #:read
	   #:write
	   #:close))
