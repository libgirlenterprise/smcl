
(in-package :cl-user)

(defpackage com.libgirl.smcl-test.smcl
  (:use :cl :com.libgirl.smcl-test :com.libgirl.smcl :prove)
  (:import-from :com.libgirl.smcl-test :*test-export-to-file-filepath*
		:make-test-thread :run-smcl-steps-and-join-thread))

(in-package :com.libgirl.smcl-test.smcl)

(defparameter *simple-data-primitive-only-list-quote*  '((:x nil
							  (:0
							   :0)
							  (:list-quote
							   (:list-quote (:y :a :b)
							    :y)
							   :c))
							 (:y (:p1 :p2)
							  (:yp
							   :yp)
							  (:list-quote :p1 :p2))
							 (:a (:p1 :p2)
							  (:a1
							   :0)
							  (:list-quote :0 (:y :d)))
							 (:c nil
							  (:0
							   :0)
							  :e)
							 (:e (:p1)
							  (:0
							   :e2)
							  (:p1 (:list-quote :p1 :1)))
							 (:f (:x)
							  (:f1
							   :f2)
							  (:e (:y :x)))))

(plan 2)
(subtest "test basic :list-quote"
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
  (finalize))

(subtest "test API with list-quote"
  (plan 38)
  (smcl-thread-run *simple-data-primitive-only-list-quote*)
  (is (smcl-get-char) #\0)
  (smcl-run-steps)
  (is (smcl-get-char) #\0)
  (smcl-run-steps)
  (is (smcl-get-char) #\0)
  (smcl-run-steps :step-count 2)
  (is (smcl-get-char) #\y)
  (smcl-run-steps)
  (is (smcl-get-char) #\p)
  (smcl-run-steps :step-count 5)
  (is (smcl-get-char) #\y)
  (smcl-run-steps :step-count 14)
  (is (smcl-get-char) #\e)
  (smcl-run-steps)
  (is (smcl-get-char) #\2)
  (smcl-run-steps :step-count 9)
  (is (smcl-get-char) #\f)
  (smcl-run-steps)
  (is (smcl-get-char) #\2)

  (smcl-run-steps :step-count 5) ;now on first list-qoute of procedure body with procedure name x
  (is (smcl-get-char) #\-)

  (smcl-run-steps :step-count 7) ;now on first list-qoute of procedure body with procedure name y
  (is (smcl-get-char) #\i)

  (smcl-run-steps :step-count 9) ;now on first list-qoute of procedure body with procedure name a
  (is (smcl-get-char) #\l)

  (smcl-run-steps :step-count 10) ;procedure c
  (is (smcl-get-char) #\e)
  
  (smcl-thread-end)
  (ok (not (sb-thread:thread-alive-p com.libgirl.smcl::*smcl-thread*)))

  (smcl-thread-run *simple-data-primitive-only-list-quote*)
  (smcl-run-steps :step-count 36)
  (is (smcl-get-char) #\l)

  (smcl-run-steps :step-count 30)
  (is (smcl-get-char) #\e)

  (smcl-run-steps)
  (is (smcl-get-char) #\p)

  (smcl-run-steps :step-count 2)
  (is (smcl-get-char) #\0)

  (smcl-run-steps)
  (is (smcl-get-char) #\0)

  (smcl-run-steps)
  (is (smcl-get-char) #\l)

  (smcl-run-steps :step-count 10)
  (is (smcl-get-char) #\0)

  (smcl-run-steps)
  (is (smcl-get-char) #\0)

  (smcl-run-steps)
  (is (smcl-get-char) #\p)

  (smcl-run-steps)
  (is (smcl-get-char) #\1)

  (smcl-run-steps)
  (is (smcl-get-char) #\e)
  
  (smcl-run-steps)
  (is (smcl-get-char) #\p)

  (smcl-run-steps)
  (is (smcl-get-char) #\1)

  (smcl-run-steps)
  (is (smcl-get-char) #\y)

  (smcl-run-steps)
  (is (smcl-get-char) #\l)

  (smcl-run-steps :step-count 10)
  (is (smcl-get-char) #\x)

  (smcl-run-steps)
  (is (smcl-get-char) #\l)

  (smcl-run-steps :step-count 10) ; y in f replaced
  (is (smcl-get-char) #\l) ; different from old-body, reduce-f again

  (smcl-run-steps :step-count 10) ; e in f replaced
  (is (smcl-get-char) #\l) ; different from old-body, reduce-f again.

  (smcl-run-steps :step-count 10) ; next loop of reduction from procedure x.
  (is (smcl-get-char) #\l)  

  (smcl-run-steps :step-count 30)
  (is (smcl-get-char) #\0) ; now c is 0

  (smcl-export *test-export-to-file-filepath*)
  (with-open-file (file-stream *test-export-to-file-filepath*)
    (let ((procedure-f-export))
      (dotimes (i 6 t)
	(setf procedure-f-export (read file-stream)))
      (is-print (princ (fourth procedure-f-export))
		(format nil
			"~:@(~a~)"
			"(list-quote (list-quote (list-quote x f1) 1) f1)"))))
  
  (smcl-thread-end)
  (ok (not (sb-thread:thread-alive-p com.libgirl.smcl::*smcl-thread*)))

  (finalize))

(finalize)
