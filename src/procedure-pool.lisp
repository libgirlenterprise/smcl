
(:in-package :cl-user)
(defpackage com.libgirl.smcl
  (:use :cl))

(:in-package :com.libgirl.smcl)

(defparameter *param-size* 2)

(defparameter *arg-size* 2)

(defstruct (procedure)
  (params :type list)
  (args :type list (make-list *arg-size* :initial-element '0))
  (body :type list))
	   
