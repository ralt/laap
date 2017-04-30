(defsystem #:laap
  :description "Foo"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:cffi :cl-coroutine :bordeaux-threads :uiop :cl-ppcre)
  :components ((:module "core"
		:components ((:file "package")
			     (:file "laap" :depends-on ("package" "loop" "timer"))
			     (:file "cffi" :depends-on ("package"))
			     (:file "timer" :depends-on ("package" "cffi"))
			     (:file "loop" :depends-on ("package" "cffi" "timer"))))
	       (:module "socket"
		:components ((:file "package")
			     (:file "socket")))))
