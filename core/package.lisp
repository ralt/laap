(defpackage #:laap
  (:use #:cl)
  (:export #:with-magic
	   #:deflaap
	   #:delay
	   #:spawn
	   #:defunpublic
	   #:defmethodpublic
	   #:timer
	   #:handle-error
	   #:handle-event
	   #:add-timer
	   #:remove-timer
	   #:*loop*
	   #:callback
	   #:direction
	   #:fd
	   #:closed))
