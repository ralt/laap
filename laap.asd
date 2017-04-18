(defsystem #:laap
  :description "Foo"
  :author "Florian Margaine <florian@margaine.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:cffi)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "laap" :depends-on ("package" "loop"))
			     (:file "gettimeofday" :depends-on ("package"))
			     (:file "loop" :depends-on ("package" "gettimeofday"))))))
