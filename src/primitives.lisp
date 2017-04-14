(in-package :com.libgirl.smcl)


(defparameter primitives nil)

(setf primitives (make-hash-table))
;; 11 22 -> (LIST-QUOTE 11 22)
(setf (gethash :list-quote primitives)
      (lambda (params)
	(cons :list-quote params)))
(setf (gethash :cons primitives)
      (lambda (params)
	(cons :list-quote params)))
;; (:list-quote '11 '22) -> '11, that's just shaka wants
;; because car just use one param, so we choice the second one of that param
;; but if the first one is the parameter of procedure, we should take it.
(setf (gethash :car primitives)
      (lambda (params)
	(if (listp (first params))
	    (if (equal (first (first params)) :list-quote)
		(second (first params))
		(cons :car params))
	    params)))


(setf (gethash :cdr primitives)
      (lambda (params) 
	(if (listp (first params))
	    (if (equal (first (first params)) :list-quote)
		(third (first params))
		(cons :cdr params))     ; return original form
	    :none)))	        	; if cdr a non-list-quote list, it will return :none
(setf (gethash :when primitives)
      (lambda (params default-args procedure procedure-pool)
	(if (not (equal :none
			(reduce-f (first params) procedure  procedure-pool)))  ;use our :true?? yes, but not :none is :true
	    (reduce-f (second params) procedure procedure-pool)
	    :none)))
(setf (gethash :eq primitives)		
      (lambda (params)
	(if (equal (first params) (second params)) 
	    :true
	    :none)))
(setf (gethash :atom primitives)
      (lambda (params)
	(if (not (listp (first params)))
	    :true
	    :none))) 
(setf (gethash :true primitives)
      (lambda (params)
	:true))
(setf (gethash :none primitives)
      (lambda (params)
	:none))
(find nil (list 'a 'b))

(setf (gethash :defun primitives)
      (lambda (params default-args procedure procedure-pool)
	(format t "~%DEFUN START~%")
	(block defun-block
	  (let* ((first-param-reduced (progn
					(format t "reduce first-param: ~s~%  " (first params))
					(princ (reduce-f (first params) procedure procedure-pool))))
		 (name (labels
			   ((find-procedure-param (param-reduced)
			      (if (atom param-reduced)
				  (when (find param-reduced (procedure-params procedure))
				    (progn
				      (format t "~%find first-param-reduced contains ~s, is parameter in procedure-params: ~s~%  return ~s~%"
					      param-reduced
					      (procedure-params procedure)
					      (return-from defun-block (cons :defun
									     (list first-param-reduced (second params)))))
				      (return-from defun-block (cons :defun
								     (list first-param-reduced (second params))))))
				  (progn
				    (find-procedure-param (car param-reduced))
				    (find-procedure-param (cdr param-reduced)))))
			    
			    (find-name (first-param-reduced params) 
			      (if (listp first-param-reduced) ; find name recursive
				  (if (equalp (first first-param-reduced) :list-quote)
				      (if (second first-param-reduced)
					  (find-name (second first-param-reduced) params)
					  (error ":defun Find name problem: (second first-param-reduced) is nil but is list"))
				      (return-from defun-block (cons :defun
								     (list first-param-reduced (second params)))))
				  (if (find first-param-reduced (procedure-params procedure))
				      (progn
					(format t "~%first-param-reduce is not a list and have the same parameter in procedure-params: ~s~%  return ~s~%" (procedure-params procedure)
						(cons :defun (list first-param-reduced (second params)))) 
					(return-from defun-block (cons :defun
								       (list first-param-reduced (second params)))))
				      first-param-reduced))))
			 (format t "~%find-procedure-param: ~s~%  " first-param-reduced)
			 (find-procedure-param first-param-reduced)
			 (format t "~%find-name~%")
			 (find-name first-param-reduced params)))
		 (body (second params))) ;;if body contain parameter in procedure, return (:defun params)
	    (if (primitivep name)
		;; apply directly
		(apply-primitive-f name (list body (first default-args)) default-args procedure procedure-pool)
		;; re-defun
		(let* ((procedure-ingredient-list (create-procedure-ingredient-list first-param-reduced default-args))
		       (parameter-count (get-parameter-count procedure-ingredient-list))
		       (unprimitive-symbol-list (find-unprimitive-symbol body (procedure-params procedure)))
		       (new-params nil))
		  (if procedure-ingredient-list
		      (progn
			(unless (listp (print unprimitive-symbol-list))
			  (progn
			    (format t "~%find procedure parameter in unprimitive-symbol-list~%  return ~s" (cons :defun
														 (list first-param-reduced (second params)))) 
			    (return-from defun-block (cons :defun
							   (list first-param-reduced (second params))))))
			(format t "~%parameter-count: ~s, unprimitive-symbol-count: ~s~%" parameter-count (length unprimitive-symbol-list))
			(if (or (= parameter-count 0) (= (length unprimitive-symbol-list) 0))
			    (progn
			      (format t "set-procedure~%  name: ~s~%  params: nil~%  args: ~s~%  body: ~s~%" name default-args body)
			      (set-procedure name nil (copy-tree default-args) (copy-tree body) procedure-pool))
			    (let ((params (match-params-and-unprimitive-symbols procedure-ingredient-list
										unprimitive-symbol-list))
				  (args (list (third procedure-ingredient-list) (fifth procedure-ingredient-list))))
			      (format t "set-procedure~%  name: ~s~%  params: ~s~%  args: ~s~%  body: ~s~%" name params args body)
			      (set-procedure name (copy-tree params) (copy-tree args) (copy-tree body) procedure-pool)))
			name)
		      (cons :defun params)))))))) ; If first-param-reduced contains not-list-quote list, return (:defun params)

 

;; retun a cons list or nil
(defun create-procedure-ingredient-list (first-param-reduced default-args)
  (format t "~%create procedure ingredient list~%  first-param-reduced: ~s~%  default-args: ~s~%  " first-param-reduced default-args)
 (princ (let ((default-param-args (list :name nil (first default-args) nil (second default-args)))
	(flated-params (flate-param first-param-reduced)))
    (when flated-params 
      (labels ((compose-list (flated-params param-args)
		 (if (and flated-params param-args)
		     (append (list (car flated-params)) (compose-list (cdr flated-params) (cdr param-args)))
		     param-args)))
	(compose-list flated-params default-param-args))))))


;; all result is a list or nil
;; Be careful second and third may not suitible for the future
(defun flate-param (param)
  (labels ((recursive-flate-param (param)
	     (if (atom param)
		 (list param)
		 (if (equal (first param) :list-quote)
		     (append (flate-param (second param)) (flate-param (third param)))
		     (return-from flate-param nil)))))
    (recursive-flate-param param)))


(defun get-parameter-count (procedure-ingredient-list)
  (format t "~%get parameter count~%  ")
  (princ
   (case (count nil procedure-ingredient-list)
     (0 2)
     (1 1)
     (2 0))))

(defun find-unprimitive-symbol (body procedure-params)
  (format t "~%find unprimitive symbol from body: ~s~%  " body)
  (princ (labels ((recursive-find-unprimitive-symbol (body)
		    (print body)
		    (let ((body-list (if (atom body)
					 (list body)
					 body)))
		      (labels ((adjoin-list (body-list)
				 (iter (for item in body-list)
				   (if (atom item)
				       (if (find item (print procedure-params))
					   (return-from find-unprimitive-symbol t)
					   (if (not (primitivep item))
					       (adjoining item)))
				       (unioning (adjoin-list item))))))
			(adjoin-list body-list)))))
    (recursive-find-unprimitive-symbol body))))



(defun parse-symbol-to-number (symbol)
  (format t "~%  parse symbol to number~%  ")
  (princ
   (let ((sha1output (make-string-output-stream))
	 (aa (make-string-input-stream (symbol-name symbol))))
     (sb-ext:run-program "/usr/bin/openssl"
			 (list "sha1")
			 :INPUT aa
			 :output sha1output)
     (parse-integer (get-output-stream-string sha1output) :radix 16))))


(defun match-params-and-unprimitive-symbols (procedure-ingredient-list unprimitive-symbol-list)
  (format t "match params and unprimitive symbols~%  procedure-ingredient-list: ~s~%  unprimitive-symbol-list: ~s" procedure-ingredient-list unprimitive-symbol-list)
  (cond ((not (second procedure-ingredient-list))
	 (error "second parameter in procedure-ingredient-list should not be nil. Because if second parameter is nil, it should be directly set-procedure"))
	((= (length unprimitive-symbol-list) 0)
	 (error "unprimitive-symbol-list should not be 0"))
	(t (let ((params (append (list (second procedure-ingredient-list)) 
				 (if (fourth procedure-ingredient-list)
				     (list (fourth procedure-ingredient-list))))))
	     (format t "~%  params in new procedure: ~s" params)
	     (labels ((match-them (params unprimitive-symbol-list)
			(if (and (car params) (car unprimitive-symbol-list))
			    (let ((parameter-choiced (nth (mod (parse-symbol-to-number (car params))
							       (length unprimitive-symbol-list))
							  unprimitive-symbol-list)))
			      (cons parameter-choiced
				    (match-them (cdr params) (remove parameter-choiced
								     unprimitive-symbol-list))))
			    nil)))
	       (let ((parameter-choiced-list (match-them params unprimitive-symbol-list)))
		 (format t "~%  ~s~%" parameter-choiced-list)
		 parameter-choiced-list))))))



(defun primitivep (procedure-name)
  (if (gethash procedure-name primitives)
      t))


(defun special-primitive-p (procedure-name)
  (find procedure-name
	'(:when :defun :list-quote)))

(defun apply-primitive-f (primitive-name params default-args procedure procedure-pool)
  (format t "~%apply-primitive-f  ~s" primitive-name)
  (format t "~%  params: ~s~%  args: ~s~%  " params default-args)
  (princ (if (not (primitivep primitive-name))
	     (error "Apply Non-primitive Error")
	     (if (or (equal primitive-name :defun)
		     (equal primitive-name :when))
		 (funcall (gethash primitive-name primitives) params default-args procedure procedure-pool)
		 (funcall (gethash primitive-name primitives) params)))))







