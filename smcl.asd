
(in-package :cl-user)

(defpackage com.libgirl.smcl-asd
  (:use :cl :asdf))
(in-package :com.libgirl.smcl-asd)

(defsystem smcl
  :components ((:module "src"
		:serial t
		:components ((:file "procedure-pool")
			     (:file "primitives")
			     (:file "smcl")))))
		
