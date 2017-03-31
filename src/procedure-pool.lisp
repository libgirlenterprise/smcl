;;;; TODO:
;;;; 1. exception handling

(in-package :cl-user)
(defpackage com.libgirl.smcl
  (:use :cl :iterate)
  (:export :smcl-run))

(in-package :com.libgirl.smcl)

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
  (body nil)) ; should handle null case or any atom in list is not a symbol

(defclass procedure-pool ()
  ((procedures :type hash-table
	       :initform (make-hash-table))))

(defgeneric reduce-f (body procedure-or-procedure-pool &optional procedure-pool-or-unused))

(defgeneric invoke-f (symbol args procedure-pool)) ; should handle when symbol is not a symbol

(defgeneric set-procedure (name params args body procedure-pool))

(defgeneric export-to-file (file-pathname procedure-pool))

(defgeneric get-procedure (name procedure-pool))

(defmethod get-procedure (name (procedure-pool procedure-pool))
  (gethash name
	   (slot-value procedure-pool 'procedures)))

(defmethod initialize-instance :after ((procedure-pool procedure-pool) &key init-procedures)
  (dolist (procedure-form init-procedures)
    (when (and procedure-form
	       (first procedure-form)
	       (symbolp (first procedure-form)) ; name should be symbol
	       (every #'symbolp ; all non nil params should be symbols
		      (second procedure-form)) ; WARNING: we haven't check the param size
	       (third procedure-form) ; default arguments for this procedures
	       (= *arg-size* (length (third procedure-form))) ; WARNING: we don't check the format of each default argument temporarily
	       (fourth procedure-form)) ; body, WARNING: we don't check the format temporarily
      (apply #'set-procedure (append (copy-tree procedure-form)
				     (list procedure-pool))))))

(defmethod reduce-f (name (procedure-pool procedure-pool) &optional unused-1) ; WARNING: name whould be an atom
  "Reduce the procedure body of the procedure of the name. And set the procedure body to its simpliest form"
  (if (primitivep name)
      (error "Error going to modify primitive procedure")
      (let ((procedure (get-procedure name procedure-pool)))
	(prog1
	    (if procedure
		(setf (procedure-body procedure)
		      (reduce-f (procedure-body procedure)
				procedure
				procedure-pool))
		name)
	  (funcall *user-input-function*)))))

(defmethod reduce-f (body procedure &optional (procedure-pool procedure-pool))
  "Reduce the body to its simpliest form (perfect form)."
  (let ((body-operator (if (atom body)
			   body ; now the same as body unless recursion
			   (if (atom (first body))
			       (first body)
			       :list-quote)))); even if the first element of the body is not an atom, it should be perfect reduced
    (progn
      ;; reduce sub-procedure before invoke
      (unless (primitivep body-operator)
	(reduce-f body-operator procedure-pool))

      ;; unless the operator is special-primitive-p, reduce arguments of this body
      (unless (or (atom body)
		  (= (length body) 1)
		  (special-primitive-p body-operator))
	(do* ((i 1 (incf i)))
	     ((or (atom body) ; for the case body being rewriten
		  (>= i (length body))))
	  (setf (nth i body)
		(reduce-f (nth i body)
			  procedure
			  procedure-pool))
	  (funcall *user-input-function*)))
      
      (if (find body-operator
		(procedure-params procedure)) ; it means body-operator not determined because it is a parameter. This case body is already perfect
	  body
	  ;; invoke sub-procedure
	  ;; and reduce body again until perfect form
	  ;; WARNING: doesn't clear about the case being rewriten
	  (let ((new-body (apply (if (primitivep body-operator)
				     #'apply-primitive-f
				     #'invoke-f)
				 (append (list body-operator)
					 (list (subseq (append (unless (atom body)						      
								 (subseq body 1))
							       (copy-tree (procedure-args procedure)))
						       0
						       *arg-size*))
					 (when (primitivep body-operator)
					   (list (copy-tree (procedure-args procedure))
						 procedure))
					 (list procedure-pool)))))
	    (if (equalp body new-body)
		body
		(reduce-f new-body procedure procedure-pool)))))))

(defmethod invoke-f (symbol args (procedure-pool procedure-pool))
  (let* ((procedure (get-procedure symbol procedure-pool)))
    (if procedure ; WARNING: in current version, new-body shouldn't be nil when procedure = t. it could change in the future.
	(let ((new-body (if (listp (procedure-body procedure))
			    (copy-tree (procedure-body procedure))
			    (list (procedure-body procedure)))))
	  (labels ((replace-params-by-args (params args body-list)
		     (loop for sub-body in body-list
			   collect (or (when params
					 (some #'(lambda (param item arg)
						   (when (eq param item)
						     (if (atom arg)
							 arg
							 (copy-tree arg))))							 
					       params
					       (make-list *arg-size* :initial-element sub-body)
					       args))
				       (if (atom sub-body)
					   sub-body
					   (replace-params-by-args params args sub-body))))))
	    (replace-params-by-args (procedure-params procedure)
				    args
				    new-body)))
	symbol)))

(defmethod set-procedure (name params args body (procedure-pool procedure-pool))
  (let* ((procedure (get-procedure name procedure-pool)) ; TODO & WARNING: handle the case when name is not a symbol
	 (procedure-new-created-p (null procedure))
	 (procedure (or procedure
			(make-procedure)))) 
    (setf (procedure-params procedure)
	  params)
    (setf (procedure-args procedure)
	  (mapcar (lambda (arg)
		    (or (reduce-f arg
				  (make-procedure) ; use an isolated procedure 
				  (make-instance 'procedure-pool)) ; and an isolated procedure-pool to reduce args
			arg))
		  args))
    (setf (procedure-body procedure) body) ;WARNING: should handle the case that body is not a proper format?
    (when procedure-new-created-p
      (setf (gethash name
		     (slot-value procedure-pool 'procedures))
	    procedure))))

(defmethod export-to-file (file-pathname (procedure-pool procedure-pool))
  (with-open-file (file-output-stream (pathname file-pathname)
				      :direction :output
				      :if-exists :supersede)
    (loop for procedure-name being the hash-keys in (slot-value procedure-pool 'procedures)
	  do (let ((procedure (get-procedure procedure-name procedure-pool)))
	       (format file-output-stream ; TODO: reindent file
		       "~(~a~)" ; convert to lowercase
		       (format nil
			       "(~a ~a ~% (~{~a~%  ~})~% ~a)~%~%"
			       procedure-name
			       (or (procedure-params procedure); WARNING: Do we have to check if it's a list?e
				   "()")
			       (procedure-args procedure)
			       (procedure-body procedure)))))))

