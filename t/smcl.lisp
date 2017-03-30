
(in-package :cl-user)

(defpackage com.libgirl.smcl-test.smcl
  (:use :cl :com.libgirl.smcl-test :prove)
  (:import-from :com.libgirl.smcl-test :*test-export-to-file-filepath*))

(in-package :com.libgirl.smcl-test.smcl)

(plan 1)
(let ((cl-user::procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool
					      :init-procedures (list (list :a
									   nil
									   (list :a1 :a2)
									   (list :list-quote :x :y))))))
  (is (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :a
												 cl-user::procedure-pool))
			      (com.libgirl.smcl::get-procedure :a cl-user::procedure-pool)
			      cl-user::procedure-pool)
      (list :list-quote :x :y)
      :test #'equalp))
(finalize)
								
