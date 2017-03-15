


(:in-package :com.libgirl.smcl)
  

(let ((primitives nil))
  (setf primitives (make-hash-table))
  (setf (gethash 'quote primitives)
        (lambda (arg-pars-x arg-pars-y arg-deft-1 arg-deft-2 proc-pool)
	  (list 'quote
		(reduce-f arg-pars-x
			  (make-procedure :args (list arg-deft-1 arg-deft-2) :body arg-pars-x)
			  proc-pool)
		(reduce-f arg-pars-y
			  (make-procedure :args (list arg-deft-1 arg-deft-2) :body arg-pars-y)
			  proc-pool))))
  (setf (gethash 'cons primitives)
	(lambda (arg-pars-x arg-pars-y arg-deft-1 arg-deft-2 proc-pool)
	  (list 'quote
		(reduce-f arg-pars-x
			  (make-procedure :args (list arg-deft-1 arg-deft-2) :body arg-pars-x)
			  proc-pool)
		(reduce-f arg-pars-y
			  (make-procedure :args (list arg-deft-1 arg-deft-2) :body arg-pars-y)
			  proc-pool))))
  
  (Funcall (gethash 'quote primitives) 22 44 66 88)
  )

(defun primitivep (proc)
  (if (gethash proc primitives)
      t))
(defun appy-primitive (prim-name arg-parameters arg-defauls proc-pool)
  ((gethash prim-name primitives) (car arg-pars) (cdr arg-pars) (car arg-defauls) (cdr arg-defauls) proc-pool))


      
  




(:in-package :com.libgirl.smcl)
  

(let ((primitives nil))
  (setf primitives (make-hash-table))
  (setf (gethash 'quote primitives)
        (lambda (arg-p-x arg-p-y arg-d-1 arg-d-2 proc-pool)
	  (list 'quote arg-p-x arg-p-y)))
  (setf (gethash 'cons primitives)
	(lambda (arg-p-x arg-p-y arg-d-1 arg-d-2 proc-pool)
	  (list 'quote arg-p-x arg-p-y)))
  (setf (gethash 'car primitives)
	(lambda (arg-p-x arg-p-y arg-d-1 arg-d-2 proc-pool)
	  (list 'quote (if (listp arg-p-x)
			   (car arg-p-x)
			   arg-p-x))))
  (setf (gethash 'cdr primitives)
	(lambda (arg-p-x arg-p-y arg-d-1 arg-d-2 proc-pool)
	  (list 'quote (if (listp arg-p-x)
			   (cdr arg-p-x)
			   nil))))
  
  (funcall (gethash 'quote  primitives) '(11 22) 33 99 44 55)
  )

(defun primitivep (proc-name)
  (if (gethash proc primitives)
      t))
(defun special-primitive-p (proc-name)
  (equal proc-name 'when))
    
(defun appy-primitive (primitive-name arg-parameters arg-defauls proc-pool)
  ((gethash prim-name primitives) (car arg-pars) (cdr arg-pars) (car arg-defauls) (cdr arg-defauls) proc-pool))



      
  


  

