; (:in-package :com.libgirl.smcl)
  
(defparameter *primitives* nil)

(setf primitives (make-hash-table))
					;11 22 -> (LIST-QUOTE 11 22)
(setf (gethash 'list-quote primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2)
	(list 'list-quote param-x param-y)))
(setf (gethash 'cons primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2)
	(list 'list-quote param-x param-y)))
					;('list-quote '11 '22) -> '11, that's just shaka wants
(setf (gethash 'car primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2)
	(list 'quote (if (listp param-x)
			 (second param-x)
			 param-x))))
(setf (gethash 'cdr primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2) 
	(list 'quote (if (listp param-x)
			 (cdr param-x) ;not (cdr (second param-x))??
			 'nil))))
(setf (gethash 'when primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2 procedure procedure-pool)
	(list 'list-quote (if (reduce-f param-x procedure procedure-pool) ;Is 'list-quote necessary here?
			      (reduce-f param-y procedure procedure-pool)))))
(setf (gethash 'eq primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2)
	(if (equal param-x param-y)
	    't
	    'nil)))
(setf (gethash 'atom primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2)
	(if (not (listp param-x))
	    't
	    'nil)))
(setf (gethash 't primitives) 't)
(setf (gethash 'nil primitives) 'nil)
(setf (gethash 'defun primitives)
      (lambda (param-x param-y default-arg-1 default-arg-2 procedure procedure-pool)
	(let* ((param-a (reduce-f param-x procedure procedure-pool))
	       (name (if (listp param-a)
			 (if (listp (second param-a))
			     (first (second param-a))
			     (second param-a))
			 param-a))
	       (body param-y))
	  (if (primitivep name)
	      (apply-primitive name (list body default-arg-1) (list default-arg-1 default-arg-2) procedure procedure-pool)
	      (create-procedure-ingredient-list param-a default-arg-1 default-arg-2)
	      ))))
  




(defun primitivep (procedure-name)
  (if (gethash procedure-name primitives)
      t))

(defun special-primitive-p (procedure-name)
  (or (equal procedure-name 'when)
      (equal procedure-name 'defun)))
    
(defun apply-primitive (primitive-name params default-args procedure procedure-pool)
  (if (not (primitivep primitive-name))
      (error "Apply Non-primitive Error")
      (if (special-primitive-p primitive-name)
	  (funcall (gethash primitive-name primitives) (car params) (cdr params) (car default-args) (cdr default-args) prcedure procedure-pool)
	  (funcall (gethash primitive-name primitives) (car params) (cdr params) (car default-args) (cdr default-args)))))



;retun a cons list
(defun create-procedure-ingredient-list (param-a default-arg-1 default-arg-2)
  (let ((default-param-args (list :none default-arg-1 :none default-arg-2))
	(flated-a (flate-param param-a)))
    (labels ((create-list (param-a param-args)
	       (if (and (atom param-a)  param-args)
		   param-args
		   (cons (car param-a) (create-list (cdr param-a) (cdr param-args)))
		   )))
      (create-list param-a default-param-args))))


(defun find-unprimitive-symbol (body)
  (if (atom body)
      (if (primitivep body)
	  ;; find the other way
	  body)
      (cons (find-unprimitive-symbol (car body)) (find-unprimitive-symbol (cdr body)))))

(defun flate-param (param)
  (if (listp param)
      (append (flate-param (second param)) (flate-param (third param)))
      param))

	
;Test 'car
;; ((lambda (param-x param-y default-arg-1 default-arg-2) 
;;   (list 'quote (if (listp param-x)
;; 		   (car param-x)
;; 		   param-x)))
;;  (list 'LIST-QUOTE 11 22) 33 44 55)
					;Test 'cdr
;; (lambda (param-x param-y default-arg-1 default-arg-2) 
;;    (list 'quote (if (listp param-x)
;; 		    (cdr param-x) 
;; 		    'nil)))



(defun test-create-procedure-ingredient-list ()
  (print (create-procedure-ingredient-list 'list-quote 'arg1 'arg2))
   (create-procedure-ingredient-list (list 'list-quote 'X1 'X2) 'arg1 'ar2))
  
		   
  
