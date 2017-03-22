(in-package :cl-user)
(defpackage com.libgirl.smcl-test.procedure-pool
  (:use :cl :com.libgirl.smcl :prove))
(in-package :com.libgirl.smcl-test.procedure-pool)

(defparameter *non-export-symbol-list* '("*PARAM-SIZE*"
					 "*ARG-SIZE*"
					 "*USER-INPUT-FUNCTION*"
					 "MAKE-USER-INPUT-FUNCTION"
					 "PROCEDURE"
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
(plan (+ 1 (length *non-export-symbol-list*)))
(ok (every #'(lambda (symbol)
	       (ok (not (find-symbol symbol))))
	   *non-export-symbol-list*))
(plan (+ 1 (* 2 (length *non-export-symbol-list*))))
(ok (every #'(lambda (symbol)
	       (multiple-value-bind (when-symbol status)
		   (find-symbol symbol 'com.libgirl.smcl)
		 (ok when-symbol)
		 (is status :internal)))
	   *non-export-symbol-list*))
(plan 1)
(ok (find-symbol "SMCL-RUN"))


(plan 1)

;;;null case
(let ((empty-procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool)))
  (is-type empty-procedure-pool 'com.libgirl.smcl::procedure-pool))

(finalize)
  
