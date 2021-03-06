(defsystem #:laap-test
  :description "Bar"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:laap :babel :cffi :cl-base32)
  :components ((:module "t"
		:components ((:file "package")
			     (:file "laap" :depends-on ("package" "test"))
			     (:file "socket" :depends-on ("package" "test"))
			     (:file "fs" :depends-on ("package" "test"))
			     (:file "test" :depends-on ("package"))))))
