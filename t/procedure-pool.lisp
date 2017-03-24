(in-package :cl-user)
(ql:quickload "smcl")

(defpackage com.libgirl.smcl-test.procedure-pool
  (:use :cl :com.libgirl.smcl :prove))
(in-package :com.libgirl.smcl-test.procedure-pool)

(defparameter *non-export-symbol-list* '("*PARAM-SIZE*"
					 "*ARG-SIZE*"
					 "*USER-INPUT-FUNCTION*"
					 "MAKE-USER-INPUT-FUNCTION"
					 "PROCEDURE"
					 "PROCEDURE-PARAMS"
					 "PROCEDURE-ARGS"
					 "PROCEDURE-BODY"
					 "PROCEDURE-POOL"
					 "PROCEDURES"
					 "REDUCE-F"
					 "INVOKE-F"
					 "SET-PROCEDURE"
					 "EXPORT-TO-FILE"))

(defparameter subtest-number-list (list (+ 1 (* 2 (length *non-export-symbol-list*)))
					4
					6))

(defun run-program-for-make-user-input-function (input-stream)
  (sb-ext:run-program "/usr/local/bin/sbcl"
		      '("--noinform" "--load" "make-user-input-function.lisp")
		      :output nil
		      :input input-stream
		      :wait nil))

(plan 3)

(subtest "test export or non-export of com.libgirl.smcl"
  (plan (first subtest-number-list))
  (mapcar #'(lambda (symbol)
	      (ok (not (find-symbol symbol)))
	      (is-values (find-symbol symbol 'com.libgirl.smcl)
			 (list (intern symbol 'com.libgirl.smcl)
			       :internal)))
	  *non-export-symbol-list*)
  (ok (find-symbol "SMCL-RUN"))
  (finalize))


(subtest "null cases"
  (plan (second subtest-number-list))
  (let* ((empty-procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool))
	 (cl-user::procedures (slot-value empty-procedure-pool 'com.libgirl.smcl::procedures)))
    (is-type empty-procedure-pool 'com.libgirl.smcl::procedure-pool)
    (is-type cl-user::procedures 'hash-table)
    (is (hash-table-count cl-user::procedures) 0)
    (is (com.libgirl.smcl::reduce-f 'x
				    (com.libgirl.smcl::make-procedure)
				    empty-procedure-pool)
	'x))
  (finalize))


(subtest "simple case"
  (plan (third subtest-number-list))
  (setf com.libgirl.smcl::*user-input-function*
	(lambda () ()))

  (let* ((cl-user::procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool))
	 (cl-user::procedures (slot-value cl-user::procedure-pool 'com.libgirl.smcl::procedures)))
    (mapcar #'com.libgirl.smcl::set-procedure
	    (list 'x 'y 'z)
	    (list nil nil nil)
	    (make-list 3
		       :initial-element (make-list com.libgirl.smcl::*arg-size* :initial-element '0))
	    (list 'y 'z 'v)
	    (make-list 3 :initial-element cl-user::procedure-pool))
    (let ((procedure-x (gethash 'x cl-user::procedures)))
      (is (gethash 'v cl-user::procedures)
	  nil)
      (is (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body procedure-x)
				      procedure-x
				      cl-user::procedure-pool)
	  'v)
      (is (com.libgirl.smcl::procedure-body procedure-x)
	  'y)
      (is (com.libgirl.smcl::procedure-body (gethash 'y cl-user::procedures))
	  'v)
      (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body procedure-x)
				  procedure-x
				  cl-user::procedure-pool
				  :set-procedure-new-body-p t)
      (is (com.libgirl.smcl::procedure-body procedure-x)
	  'v)
      (is (gethash 'v cl-user::procedures)
	  nil)))
  (finalize))


(finalize)

