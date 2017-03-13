


(:in-package :com.libgirl.smcl)
  

(let ((primitives nil))
  (setf primitives (make-hash-table))
  (setf (gethash 'quote primitives)
        (lambda (arg-p-x arg-p-y arg-d-1 arg-d-2 proc-pool)
	  (list 'quote
		(reduce-f arg-p-x
			  (make-procedure :args (list arg-d-1 arg-d-2) :body arg-p-x)
			  proc-pool)
		(reduce-f arg-p-y
			  (make-procedure :args (list arg-d-1 arg-d-2) :body arg-p-x)
			  proc-pool))))
  (setf (gethash 'cons primitives)
	(lambda (x y a b) (list 'quote (reduce-f x proc proc-pool) (reduc-f y proc proc-pool))))
  (funcall (gethash 'quote primitives) 22 44 66 88)
  )

(defun primitivep (proc)
  (if (gethash proc primitives)
      t))
(defun appy-primitive (prim-name arg-parameters arg-defauls proc-pool)
  ((gethash prim-name primitives) (car arg-pars) (cdr arg-pars) (car arg-defauls) (cdr arg-defauls) proc-pool))


      
  


  

