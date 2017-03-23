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
					 "REDUCE-F"
					 "INVOKE-F"
					 "SET-PROCEDURE"
					 "EXPORT-TO-FILE"))

(defun run-program-for-make-user-input-function (input-stream)
  (sb-ext:run-program "/usr/local/bin/sbcl"
		      '("--noinform" "--load" "make-user-input-function.lisp")
		      :output nil
		      :input input-stream
		      :wait nil))

;;; test export or non-export of com.libgirl.smcl
(plan (* 2 (length *non-export-symbol-list*)))
(mapcar #'(lambda (symbol)
	    (ok (not (find-symbol symbol)))
	    (is-values (find-symbol symbol 'com.libgirl.smcl)
		       (list (intern symbol 'com.libgirl.smcl)
			     :internal)))
	   *non-export-symbol-list*)
(finalize)

(plan 1)
(ok (find-symbol "SMCL-RUN"))
(finalize)


;;;null case
(plan 3)
(let* ((empty-procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool))
       (procedures (slot-value empty-procedure-pool 'com.libgirl.smcl::procedures)))
  (is-type empty-procedure-pool 'com.libgirl.smcl::procedure-pool)
  (is-type procedures 'hash-table)
  (is (hash-table-count procedures) 0)
  (finalize)

  ;;test empty pool to start reduce-f
  (plan 1)
  (ok (not (com.libgirl.smcl::reduce-f 'x
		     (com.libgirl.smcl::make-procedure)
		     empty-procedure-pool)))
  (finalize))


(setf com.libgirl.smcl::*user-input-function*
      (lambda () ()))

;;; simple case
(plan 3)
(let* ((procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool))
       (procedures (slot-value procedure-pool 'com.libgirl.smcl::procedures)))
  (setf (gethash 'x procedures) (com.libgirl.smcl::make-procedure :body 'y)
	(gethash 'y procedures) (com.libgirl.smcl::make-procedure :body 'z))
  (let ((procedure-x (gethash 'x procedures)))
    (is (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body procedure-x)
				    procedure-x
				    procedure-pool)
	'z)
    (is (com.libgirl.smcl::procedure-body procedure-x)
	'y)
    (is (com.libgirl.smcl::procedure-body (gethash 'y procedures))
	'z)))
(finalize)

(unintern 'procedure-pool)
