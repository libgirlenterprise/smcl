
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

;; (smcl-thread-run '((:x nil
;; 		    (:0
;; 		     :0)
;; 		    (:list-quote
;; 		     (:list-quote (:y :a :b)
;; 		      :y)
;; 		     :c))
;; 		   (:y (:p1 :p2)
;; 		    (:yp
;; 		     :yp)
;; 		    (:list-quote :p1 :p2))
;; 		   (:a (:p1 :p2)
;; 		    (:a1
;; 		     :0)
;; 		    (:list-quote :0 (:y :d)))
;; 		   (:c nil
;; 		    (:0
;; 		     :0)
;; 		    :e)
;; 		   (:e (:p1)
;; 		    (:0
;; 		     :e2)
;; 		    (:p1 (:list-quote :p1 :1)))
;; 		   (:f (:x)
;; 		    (:f1
;; 		     :f2)
;; 		    (:e (:y :x)))))

(finalize)


		       
