
(in-package :cl-user)
(defpackage com.libgirl.smcl-test-asd
  (:use :cl :asdf))
(in-package :com.libgirl.smcl-test-asd)

(defsystem smcl-test
  :depends-on (:smcl
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
		:serial t
		:components ((:test-file "smcl-test-init")
			     (:test-file "smcl-test-config")
			     (:test-file "procedure-pool"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
