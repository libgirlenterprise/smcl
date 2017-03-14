;;;; TODO:
;;;; 1. exception handling

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

(defgeneric reduce-f (body procedure procedure-pool))

(defgeneric invoke-f (symbol args procedure-pool)) ; should handle when symbol is not a symbol

(defmethod reduce-f (body procedure (procedure-pool procedure-pool))
  (let* ((body-operator (if (atom body)
			    body ; now the same as body unless recursion
			    (if (atom (first body))
				(first body)
				'quote))) ; even if the first element of the body is not an atom, it should be perfect reduced
	 (operator-primitive-p (primitivep body-operator)))

    ;; reduce sub-procedure before invoke
    (unless operator-primitive-p
      (let ((sub-procedure (gethash body-operator
				    (slot-vaue procedure-pool 'procedures))))
	(reduce-f sub-procedure-body sub-procedure-params procedure-pool)))

    ;; reduce arguments of this body
    (unless (or (atom body)
		(= (length body) 1))
      (let ((perfect-form))
	(do* ((i 1 (incf i)))
	     ((or (atom body) ; for the case being rewriten
		  (if (>= (- i 1) (length body)) ; for the same case of the last line
		      t
		      (progn
			(setf (nth (- i 1) body)
			      perfect-form)
			nil))
		  (>= i (length body))))
	  (setf perfect-form (reduce-f (nth i body)
				       procedure
				       procedure-pool)))))

    ;; invoke sub-procedure
    ;; WARNING: doesn't clear about the case being rewriten
    (let* ((old-body (copy-list body)) 
	   (new-body (apply (if operator-primitive-p
			       #'apply-primitive-f
			       #'invoke-f)
			   (append (list body-operator)
				   (append (if (atom body)
					       nil
					       (subseq body 1))
					   (copy-list procedure-args)) ; WARNING: we might make it too long
				   (if operator-primitive-p
				       (copy-list procedure-args))
				   (list procedure-pool)))))
      (if (equalp old-body new-body)
	  new-body
	  (reduce-f new-body procedure procedure-pool)))))
	  
			    
(defmethod invoke-f (symbol args (procedure-pool procedure-pool))
  (let* ((procedure (gethash symbol
			     (slot-vaue procedure-pool 'procedures)))
	 (new-body (if (listp procedure-body)
		       (copy-list procedure-body)
		       (list procedure-body))))
    (labels ((replace-params-by-args (params args body-list)
	       (loop for sub-body in body-list
		     collect (or (reduce #'or
					 (mapcar #'(lambda (param item arg)
						     (if (eq param item)
							 (if (atom arg)
							     arg
							     (copy-list arg))
							 nil))
						 (subseq params 0 *param-size*)
						 (make-list *param-size* sub-body)
						 (subseq args 0 *param-size*)))
				 (if (atom sub-body)
				     sub-body
				     (replace-params-by-args params args sub-body))))))
      (replace-params-by-args procedure-params args new-body))))
