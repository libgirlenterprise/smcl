(in-package :cl-user)

(defpackage com.libgirl.smcl-test
  (:use :cl :com.libgirl.smcl :prove)
  (:export :*test-export-to-file-filepath*))
(in-package :com.libgirl.smcl-test)

(setf prove:*enable-colors* t)

(diag "smcl-test initialization")
(plan nil)
(defparameter *test-export-to-file-filepath* #P"~/test-export-to-file-filepath.lisp")

(finalize)



