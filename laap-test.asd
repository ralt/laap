(defsystem #:laap-test
  :description "Bar"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT"
  :serial t
  :depends-on (:laap :fiveam)
  :components ((:module "t"
		:components ((:file "package")
			     (:file "laap" :depends-on ("package"))
			     (:file "socket" :depends-on ("package"))))))
