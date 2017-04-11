(in-package :com.libgirl.smcl)


(defparameter primitives nil)

(setf primitives (make-hash-table))
;;11 22 -> (LIST-QUOTE 11 22)
(setf (gethash :list-quote primitives)
      (lambda (params)
	(cons :list-quote params)))
(setf (gethash :cons primitives)
      (lambda (params)
	(cons :list-quote (cons car-params cdr-params))))


;;(:list-quote '11 '22) -> '11, that's just shaka wants
;;because car just use one param, so we choice the second one of that param
;;but if the first one is the parameter of procedure, we should take it.
(setf (gethash :car primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2)
	(if (listp car-params)
	    (if (equal (first car-params) :list-quote)
		(second car-params)
		:not-yet)
	    car-params)))


(setf (gethash :cdr primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2) 
	(if (listp car-params)
	    (if (equal (first car-params) :list-quote)
		(third car-params)
		:not-yet);return original form (:cdr (car-params cdr-params))
	    :none)))		;if cdr a non-list-quote list, it will return :none

(setf (gethash :when primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2 procedure procedure-pool)
	(if (reduce-f car-params procedure procedure-pool) ;Is :list-quote necessary here? No ;use our :true
	    (reduce-f (car cdr-params) procedure procedure-pool)
	    :none)))
(setf (gethash :eq primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2)
	(if (equal car-params cdr-params); car cdr are different meaning, cannot compare
	    :true
	    :none)))
(setf (gethash :atom primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2)
	(if (not (listp car-params))
	    :true
	    :none))) 
(setf (gethash :true primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2)
	:true))
(setf (gethash :none primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2)
	:none))
(setf (gethash :defun primitives)
      (lambda (car-params cdr-params default-arg-1 default-arg-2 procedure procedure-pool)
	(let* ((car-params-reduced (reduce-f car-params procedure procedure-pool))
	       (name (if (listp car-params-reduced)
			 (if (listp (second car-params-reduced))
			     (first (second car-params-reduced))
			     (second car-params-reduced))
			 car-params-reduced))
	       (body cdr-params)
	       )
	  (if (primitivep name)
	      ;; apply directly
	      (apply-primitive-f name (list body default-arg-1) (list default-arg-1 default-arg-2) procedure procedure-pool)
	      ;; re-defun
	      (let* ((procedure-ingredient-list (create-procedure-ingredient-list car-params-reduced default-arg-1 default-arg-2))
		     (parameter-count (get-parameter-count procedure-ingredient-list))
		     (unprimitive-symbol-list (find-unprimitive-symbol body))
		     (unprimitive-symbol-count (length unprimitive-symbol-list))
		     (params nil))
		(if (or (= parameter-count 0) (= (length unprimitive-symbol-list) 0))
		    (set-procedure name nil (copy-tree (list default-arg-1 default-arg-2)) (copy-tree body) procedure-pool)
		    (let ((params (match-params-and-unprimitive-symbols procedure-ingredient-list
									unprimitive-symbol-list))
			  (args (list (third procedure-ingredient-list) (fifth procedure-ingredient-list))))
		      (set-procedure name (copy-tree params) (copy-tree args) (copy-tree body) procedure-pool)))
		name)))))

;;retun a cons list
(defun create-procedure-ingredient-list (car-params-reduced default-arg-1 default-arg-2)
  (let ((default-param-args (list :name :no-param default-arg-1 :no-param default-arg-2))
	(flated-a (flate-param car-params-reduced)))
    (labels ((compose-list (flated-a param-args)
	       (if (and flated-a param-args)
		   (append (list (car flated-a)) (compose-list (cdr flated-a) (cdr param-args)))
		   param-args)))
      (compose-list flated-a default-param-args))))


;;all result is a list
;;Be careful second and third may not suitible for the future
(defun flate-param (param)		
  (if (atom param)
      (list param)
      (append (flate-param (second param)) (flate-param (third param)))))

(defun get-parameter-count (procedure-ingredient-list)
  (case (count :no-param  procedure-ingredient-list)
    (0 2)
    (1 1)
    (2 0)))

(defun find-unprimitive-symbol (body)
  (let ((body-list (if (atom body)
		       (list body)
		       body)))
    (labels ((adjoin-list (body-list)
	       (iter (for item in body-list)
		 (if (atom item)
		     (if (not (primitivep item))
			 (adjoining item))
		     (unioning (adjoin-list item))))))
      (adjoin-list body-list))))



(defun parse-symbol-to-number (symbol)
  (let ((sha1output (make-string-output-stream))
	(aa (make-string-input-stream (symbol-name symbol))))
    (sb-ext:run-program "/usr/bin/openssl"
			(list "sha1")
			:INPUT aa
			:output sha1output)
    (parse-integer (get-output-stream-string sha1output) :radix 16)))


(defun match-params-and-unprimitive-symbols (procedure-ingredient-list unprimitive-symbol-list)
  (cond ((equal :no-param (second procedure-ingredient-list))
	 (error "second parameter in procedure-ingredient-list should not be :none"))
	((= (length unprimitive-symbol-list) 0)
	 (error "unprimitive-symbol-list should not be 0"))
	(t (let ((params (append (list (second procedure-ingredient-list)) 
				 (if (not (equal (fourth procedure-ingredient-list) :no-param))
				     (list (fourth procedure-ingredient-list))))))
	     (labels ((match-them (params unprimitive-symbol-list)
			(if (and (car params) (car unprimitive-symbol-list)) 
			    (cons (nth (mod (parse-symbol-to-number (car params)) (length unprimitive-symbol-list))
				       unprimitive-symbol-list)
				  (match-them (cdr params) (remove (nth (mod (parse-symbol-to-number (car params))
									     (length unprimitive-symbol-list))
									unprimitive-symbol-list)
								   unprimitive-symbol-list)))
			    nil)))
	       (match-them params unprimitive-symbol-list))))))




(defun primitivep (procedure-name)
  (if (gethash procedure-name primitives)
      t))


(defun special-primitive-p (procedure-name)
  (or (equal procedure-name :list-quote)
      (equal procedure-name :when)
      (equal procedure-name :defun)))

(defun apply-primitive-f (primitive-name params default-args procedure procedure-pool)
  (format t "~%apply-primitive-f  ~s" primitive-name)
  (format t "~%  params: ~s~%  args: ~s" params default-args)
  (print (if (not (primitivep primitive-name))
	     (error "Apply Non-primitive Error")
	     (if (equal primitive-name :defun)
		 (funcall (gethash primitive-name primitives) params default-args procedure procedure-pool)
		 (funcall (gethash primitive-name primitives) params)))))







