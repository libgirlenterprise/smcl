
(in-package :cl-user)

(defpackage com.libgirl.smcl-asd
  (:use :cl :asdf))
(in-package :com.libgirl.smcl-asd)

(defsystem smcl
  :depends-on (:iterate)
  :components ((:module "src"
		:serial t
		:components ((:file "procedure-pool")
			     (:file "primitives")
			     (:file "smcl"))))
  :in-order-to ((test-op (test-op smcl-test))))
		
