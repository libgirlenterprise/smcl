(:in-package :com.libgirl.smcl)
  

(let ((primitives nil))
  (setf primitives (make-hash-table))
  (setf (gethash 'list-quote primitives)
        (lambda (param-x param-y default-arg-1 default-arg-2)
	  (list 'quote arg-p-x param-y)))
  (setf (gethash 'cons primitives)
	(lambda (arg-p-x param-y default-arg-1 default-arg-2)
	  (list 'quote arg-p-x param-y)))
  (setf (gethash 'car primitives)
	(lambda (param-x param-y default-arg-1 default-arg-2)
	  (list 'quote (if (listp param-x)
			   (car param-x)
			   param-x))))
  (setf (gethash 'cdr primitives)
	(lambda (param-x param-y default-arg-1 default-arg-2)
	  (list 'quote (if (listp param-x)
			   (cdr param-x)
			   nil))))
  (setf (gethash 'when primitives)
	(lambda (param-x param-y default-arg-1 default-arg-2 procedure procedure-pool)
	  (list 'quote (if (reduce-f param-x procedure procedure-pool)
			   (reduce-f param-y procedure procedure-pool)))))
  
 ; (funcall (gethash 'quote  primitives) '(11 22) 33 99 44 55)
  )

(defun primitivep (procedure-name)
  (if (gethash procedure primitives)
      t))
(defun special-primitive-p (procedure-name)
  (equal procedure-name 'when))
    
(defun apply-primitive (primitive-name params default-args procedure procedure-pool)
  (if (not (primitivep primitive-name))
      (error "Apply Non-primitive Error")
      (if (special-primitive-p primitive-name)
	  ((gethash primitive-name primitives) (car params) (cdr params) (car arg-defauls) (cdr arg-defauls) prcedure procedure-pool)
	  ((gethash primitive-name primitives) (car params) (cdr params) (car arg-defauls) (cdr arg-defauls)))))

      
  


  

