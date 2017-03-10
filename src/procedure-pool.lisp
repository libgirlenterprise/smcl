;;; TODO:
;;; 1. exception handling

(:in-package :cl-user)
(defpackage com.libgirl.smcl
  (:use :cl))

(:in-package :com.libgirl.smcl)

(defparameter *param-size* 2)

(defparameter *arg-size* 2)

(defstruct procedure
  (params :type list)
  (args :type list (make-list *arg-size* :initial-element '0))
  (body :type list)) ; should handle null case or any atom in list is not a symbol

(defclass procedure-pool ()
  ((procedures :type hash-table
	       :initform (make-hash-table))))

(defgeneric reduce-f (body-list procedure procedure-pool))

(defgeneric invoke-f (symbol args procedure-pool)) ; should handle when symbol is not a symbol

(defmethod reduce-f (body-list procedure (procedure-pool procedure-pool))
  (loop for i from 1 below (length body-list)
	do (setf (nth i body-list)
		 (let* ((sub-body (nth i body-list))
			(sub-body (if (atom sub-body)
				      (list sub-body)
				      sub-body)))
		   (apply #'invoke-f
			  (list (first sub-body)
				(append (subseq sub-body 1)
					(procedure-args))
				procedure-pool))))))
					 
