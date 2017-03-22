;;;; TODO:
;;;; 1. exception handling

(in-package :cl-user)
(defpackage com.libgirl.smcl
  (:use :cl)
  (:export :smcl-run))

(in-package :com.libgirl.smcl)

(defparameter *param-size* 2)

(defparameter *arg-size* 2)

(defvar *user-input-function*)

(defun make-user-input-function (output-file-pathname procedure-pool) ;WARNING: we haven't handle file access error
  (lambda ()
    (case (read)
      ('export (export-to-file output-file-pathname procedure-pool))
      ('exit (sb-ext:exit)))))

(defstruct procedure
  (params nil :type list)
  (args (make-list *arg-size* :initial-element '0) :type list)
  (body nil :type list)) ; should handle null case or any atom in list is not a symbol

(defclass procedure-pool ()
  ((procedures :type hash-table
	       :initform (make-hash-table))))

(defgeneric reduce-f (body procedure procedure-pool))

(defgeneric invoke-f (symbol args procedure-pool)) ; should handle when symbol is not a symbol
      
(defgeneric set-procedure (name params args body procedure-pool))

(defgeneric export-to-file (file-pathname procedure-pool))

(defmethod initialize-instance :after ((procedure-pool procedure-pool) &key init-procedures)
  (dolist (procedure-form init-procedures)
    (when (and procedure-form
	       (first procedure-form)
	       (symbolp (first procedure-form)) ; name should be symbol
	       (every #'symbolp
		      (second procedure-form)) ; all non nil params should be symbols
	       (third procedure-form) ; default arguments for this procedures
	       (= *arg-size* (length (third procedure-form))) ; WARNING: we don't check the format of each default argument temporarily
	       (fourth procedure-form)) ; body, WARNING: we don't check the format temporarily
      (apply #'set-procedure (append (copy-tree procedure-form)
				     (list procedure-pool))))))

(defmethod reduce-f (body procedure (procedure-pool procedure-pool))
  "Set the object to which the body bound to its best reduced form (perfect form). Return the body object, but returning nil for no further reduction."
  (let ((body-operator (if (atom body)
			   body ; now the same as body unless recursion
			   (if (atom (first body))
			       (first body)
			       'list-quote)))) ; even if the first element of the body is not an atom, it should be perfect reduced
    (unless (find body-operator procedure-params) ; it means body-operator not determined because it is a parameter. This case body is already perfect.
      
      ;; reduce sub-procedure before invoke
      (unless (primitivep body-operator)
	(let ((sub-procedure (gethash body-operator
				      (slot-vaue procedure-pool 'procedures))))
	  (reduce-f sub-procedure-body sub-procedure procedure-pool)))

      ;; unless the operator is special-primitive-p, reduce arguments of this body
      (unless (or (atom body)
		  (= (length body) 1)
		  (special-primitive-p body-operator))
	(let ((perfect-form))
	  (do* ((i 1 (incf i)))
	       ((or (atom body) ; for the case body being rewriten
		    (>= (- i 1) (length body)) ; for the same case of the last line
		    (progn
		      (and perfect-form
			   (setf (nth (- i 1) body)
				 perfect-form)
			   (funcall *user-input-funcion*))
		      (>= i (length body)))))  
	    (setf perfect-form (reduce-f (nth i body)
					 procedure
					 procedure-pool)))))

      ;; invoke sub-procedure
      ;; and reduce body again until perfect form
      ;; WARNING: doesn't clear about the case being rewriten
      (let ((new-body (apply (if (primitivep body-operator)
				 #'apply-primitive-f
				 #'invoke-f)
			     (append (list body-operator)
				     (list (append (unless (atom body)						      
						     (subseq body 1))
						   (copy-tree procedure-args))) ; WARNING: we might make it too long
				     (when (primitivep body-operator)
				       (list (copy-tree procedure-args)
					     procedure))
				     (list procedure-pool)))))
	(when new-body
	  (unless (equalp body new-body)
	    (reduce-f new-body procedure procedure-pool)))))))

(defmethod invoke-f (symbol args (procedure-pool procedure-pool))
  (let* ((procedure (gethash symbol
			     (slot-value procedure-pool 'procedures))))
    (when procedure
      (let ((new-body (if (listp procedure-body)
			  (copy-tree procedure-body)
			  (list procedure-body))))
	(labels ((replace-params-by-args (params args body-list)
		   (loop for sub-body in body-list
			 collect (or (some #'(lambda (param item arg)
					       (when (eq param item)
						 (if (atom arg)
						     arg
						     (copy-tree arg))))							 
					   (mapcar #'list
						   (subseq params 0 *param-size*)
						   (make-list *param-size* :initial-element sub-body)
						   (subseq args 0 *param-size*)))
				     (if (atom sub-body)
					 sub-body
					 (replace-params-by-args params args sub-body))))))
	  (replace-params-by-args procedure-params args new-body))))))

(defmethod set-procedure (name params args body (procedure-pool procedure-pool))
  (let* ((procedure (gethash name ; TODO & WARNING: handle the case when name is not a symbol
			     (slot-value procedure-pool 'procedures)))
	 (procedure-new-created-p (null procedure))
	 (procedure (or procedure
			(make-procedure)))) 
    (setf procedure-params params)
    (setf procedure-args
	  (or (reduce-f args
			(make-procedure) ; use an isolated procedure 
			(make-instance 'procedure-pool)) ; and an isolated procedure-pool to reduce args
	      args))
    (setf procedure-body body) ;WARNING: should handle the case that body is not a proper format?
    (when procedure-new-created-p
      (setf (gethash name
		     (slot-value procedure-pool 'procedures))
	    procedure))))

(defmethod export-to-file (file-pathname (procedure-pool procedure-pool))
  (with-open-file (file-output-stream (make-pathname file-pathname)
				      :directiion :output
				      :if-exists :supersede)
    (loop for procedure-name being the hash-keys in (slot-value procedure-pool 'procedures)
	  do (let ((procedure (gethash procedure-name
				       (slot-value procedure-pool 'procedures))))
	       (format file-output-stream ; TODO: reindent file
		       "(~a ~a ~a ~%~a)~%~%)"
		       procedure-name
		       (or procedure-params ; WARNING: Do we have to check if it's a list?
			   "()")
		       procedure-args
		       procedure-body)))))
			   
