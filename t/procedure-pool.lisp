(in-package :cl-user)
(ql:quickload "smcl")

(defpackage com.libgirl.smcl-test.procedure-pool
  (:use :cl :com.libgirl.smcl :prove))
(in-package :com.libgirl.smcl-test.procedure-pool)

(defparameter *nil-function* (lambda () ())) ;TODO: make it a constant

(defparameter *simple-case-data-size* 3)

(defparameter *simple-case-data* (list (list 'x 'y 'z)
				       (make-list *simple-case-data-size*)
				       (make-list *simple-case-data-size*
						  :initial-element (make-list com.libgirl.smcl::*arg-size*
									      :initial-element '0))
				       (list 'y 'z 'v)))

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

(defparameter *subtest-number-list* (list (+ 1 (* 2 (length *non-export-symbol-list*)))
					  4
					  6
					  6))

(setf com.libgirl.smcl::*user-input-function*
      *nil-function*)

(defun run-program-for-make-user-input-function (input-stream)
  (sb-ext:run-program "/usr/local/bin/sbcl"
		      '("--noinform" "--load" "make-user-input-function.lisp")
		      :output nil
		      :input input-stream
		      :wait nil))

(plan (length *subtest-number-list*))

(subtest "test export or non-export of com.libgirl.smcl"
  (plan (first *subtest-number-list*))
  (mapcar #'(lambda (symbol)
	      (ok (not (find-symbol symbol)))
	      (is-values (find-symbol symbol 'com.libgirl.smcl)
			 (list (intern symbol 'com.libgirl.smcl)
			       :internal)))
	  *non-export-symbol-list*)
  (ok (find-symbol "SMCL-RUN"))
  (finalize))


(subtest "null cases"
  (plan (second *subtest-number-list*))
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


(defun test-simple-case (subtest-name subtest-number cl-user::procedure-pool)
  (subtest subtest-name
    (plan subtest-number)
    (let* ((cl-user::procedures (slot-value cl-user::procedure-pool 'com.libgirl.smcl::procedures))
	   (procedure-x (gethash 'x cl-user::procedures)))
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
	  nil))
    (finalize)))


(let ((procedure-pool-1 (make-instance 'com.libgirl.smcl::procedure-pool)))
  (apply #'mapcar
	 (cons #'com.libgirl.smcl::set-procedure
	       (append (copy-tree *simple-case-data*)
		       (list (make-list *simple-case-data-size* :initial-element procedure-pool-1)))))
  (test-simple-case "test simple case with set-procedure"
		    (third *subtest-number-list*)
		    procedure-pool-1))
(test-simple-case "test simple case wiht initialze-instance method"
		  (fourth *subtest-number-list*)
		  (make-instance 'com.libgirl.smcl::procedure-pool
				 :init-procedures (apply #'mapcar
							 (cons #'list
							       *simple-case-data*))))


(finalize)

