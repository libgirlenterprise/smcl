(:in-package :com.libgirl.smcl)
  

(let ((primitives nil))
  (setf primitives (make-hash-table))
  ;11 22 -> (LIST-QUOTE 11 22)
  (setf (gethash 'list-quote primitives)
        (lambda (param-x param-y default-arg-1 default-arg-2)
	  (list 'list-quote arg-p-x param-y)))
  (setf (gethash 'cons primitives)
	(lambda (arg-p-x param-y default-arg-1 default-arg-2)
	  (list 'list-quote arg-p-x param-y)))
  ;(11 22) -> '11, that's just shiaka wants
  (setf (gethash 'car primitives)
	(lambda (param-x param-y default-arg-1 default-arg-2)
	  (list 'quote (if (listp param-x)
			   (car param-x) 
			   param-x))))
  (setf (gethash 'cdr primitives)
	(lambda (param-x param-y default-arg-1 default-arg-2) 
	  (list 'quote (if (listp param-x)
			   (cdr param-x)
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
  ;; (setf (gethash 'defun primitives)
  ;; 	(lambda (param-x param-y default-arg-1 default-arg-2 procedure procedure-pool)
  ;; 	  (let* ((param-a (reduce-f param-x procedure procedure-pool))
  ;; 		 (name (if (listp param-a)
  ;; 			   (car param-a)
  ;; 			   param-a))
  ;; 		 (body param-y))
  ;; 	    (if (primitivep name)
  ;; 		(apply-primitive name (list body default-args-1) (list default-args-1 default-arg-2) procedure procedure-pool)
  ;; 		(
  ;; 		 )))))
  )




(defun primitivep (procedure-name)
  (if (gethash procedure primitives)
      t))

(defun special-primitive-p (procedure-name)
  (or (equal procedure-name 'when)
      (equal procedure-name 'defun)))
    
(defun apply-primitive (primitive-name params default-args procedure procedure-pool)
  (if (not (primitivep primitive-name))
      (error "Apply Non-primitive Error")
      (if (special-primitive-p primitive-name)
	  ((gethash primitive-name primitives) (car params) (cdr params) (car arg-defauls) (cdr arg-defauls) prcedure procedure-pool)
	  ((gethash primitive-name primitives) (car params) (cdr params) (car arg-defauls) (cdr arg-defauls)))))


;retun a cons list
(defun create-procedure-ingredient-list (param-a default-arg-1 default-arg-2)
  (let ((default-param-args (list :none default-arg-1 :none default-arg-2))
	(param-a-list (if (listp param-a)
			  param-a
			  (cons param-a nil))))
    (labels ((create-list (params param-args)
	       (if (and (cdr params)  param-args)
		   (cons (car params) (create-list (cdr params) (cdr param-args)))
		   (cons (car params)  param-args))))
      (create-list param-a-list default-param-args))))

;; (defun find-unprimitive-symbol (body)
;;   (if (and (atom body) body)
;;       (if (primitivep body)
;; 	  body)
;;       (cons (find-unprimitive-symbol (car body)) (find-unprimitive-symbol (cdr body)))))


	
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


