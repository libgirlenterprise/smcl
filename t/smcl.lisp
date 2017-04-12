
(in-package :cl-user)

(defpackage com.libgirl.smcl-test.smcl
  (:use :cl :com.libgirl.smcl-test :prove)
  (:import-from :com.libgirl.smcl-test :*test-export-to-file-filepath*
		:make-test-thread :run-smcl-steps-and-join-thread))

(in-package :com.libgirl.smcl-test.smcl)

(plan 1)
(make-test-thread (let ((cl-user::procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool
								:init-procedures (list (list :a
											     nil
											     (list :a1 :a2)
											     (list :list-quote :x :y))))))
		    (list (list 'is
				(cons 'list
				      (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :a
																     cl-user::procedure-pool))
								  (com.libgirl.smcl::get-procedure :a cl-user::procedure-pool)
								  cl-user::procedure-pool))
				'(list :list-quote :x :y)
				:test #'equalp))))
(run-smcl-steps-and-join-thread)
(finalize)


		       
