(in-package :cl-user)

(defpackage com.libgirl.smcl-test
  (:use :cl :com.libgirl.smcl :prove)
  (:export :*test-export-to-file-filepath*))
(in-package :com.libgirl.smcl-test)

(setf prove:*enable-colors* t)

(defmacro make-test-thread (thread-form)
  `(progn (setf com.libgirl.smcl::*smcl-thread*
		(sb-thread:make-thread (lambda ()
					 (let ((test-form-list ,thread-form))
					   (setf com.libgirl.smcl::*hand-to-api-p* t)
					   (sb-thread:signal-semaphore com.libgirl.smcl::*api-semaphore* 1)
					   (sb-thread:return-from-thread test-form-list)))))
	  (sb-thread:thread-yield)
					;	  (print (sb-thread:semaphore-count com.libgirl.smcl::*api-semaphore*))
	  (sb-thread:wait-on-semaphore com.libgirl.smcl::*api-semaphore*)))

(defun run-smcl-steps-and-join-thread()
  (let ((test-form-list))
    (loop until (setf test-form-list
		      (sb-thread:join-thread com.libgirl.smcl::*smcl-thread*
					     :default nil
					     :timeout 0.001))
	  do (smcl-run-steps))
    (mapcar #'eval test-form-list)))

(diag "smcl-test initialization")
(plan nil)
(defvar *test-export-to-file-filepath* nil)

(finalize)



