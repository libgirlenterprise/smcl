(in-package :cl-user)
(defpackage com.libgirl.smcl-test.procedure-pool
  (:use :cl :com.libgirl.smcl :prove))
(in-package :com.libgirl.smcl-test.procedure-pool)

(defun run-program-for-make-user-input-function (input-stream)
  (sb-ext:run-program "/usr/local/bin/sbcl"
		      '("--noinform" "--load" "make-user-input-function.lisp")
		      :output nil
		      :input input-stream
		      :wait nil))

(plan 1)

;;;null case
(let ((empty-procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool)))
  (is-type empty-procedure-pool 'com.libgirl.smcl::procedure-pool))

(finalize)
  
