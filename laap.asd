(defsystem #:laap
  :description "Foo"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:cffi :bordeaux-threads :uiop :cl-ppcre :cl-base32)
  :components ((:module "core"
		:components ((:file "package")
			     (:file "laap" :depends-on ("package"
							"loop"
							"timer"
							"thread-pool"))
			     (:file "cffi" :depends-on ("package"))
			     (:file "timer" :depends-on ("package"
							 "cffi"))
			     (:file "loop" :depends-on ("package"
							"cffi"
							"timer"
							"thread-pool"
							"condition"))
			     (:file "thread-pool" :depends-on ("package"
							       "condition"))
			     (:file "condition" :depends-on ("package"))))
	       (:module "socket"
		:components ((:file "package")
			     (:file "socket" :depends-on ("cffi" "package"))
			     (:file "cffi" :depends-on ("package"))))
	       (:module "fs"
		:components ((:file "package")
			     (:file "cffi" :depends-on ("package"))
			     (:file "fs" :depends-on ("cffi" "package"))))))
