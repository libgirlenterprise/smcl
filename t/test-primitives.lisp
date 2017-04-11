;; use lisp-unit



(setf com.libgirl.smcl::*user-input-function*
      (lambda () ()))

(defparameter *symbol-a* :a)
(defparameter *list-a* (list :a))
(defparameter *list-a-b* (list :list-quote :a :b))
(defparameter *list-list-c* (list :list-quote (list :list-quote :a :b) :c))
(defparameter *list-a-list* (list :list-quote :a (list :list-quote :b :c)))
(defparameter *list-list-list* (list :list-quote (list :list-quote :a :b) (list :list-quote :c :d)))
(defparameter *big-list* (list :list-quote
			       (list :list-quote
				     (list :list-quote :a :b)
				     :c)
			       (list :list-quote :d :e)))


(define-test test-primitivep
  (assert-true (com.libgirl.smcl::primitivep :list-quote))
  (assert-true (com.libgirl.smcl::primitivep :cons))
  (assert-true (com.libgirl.smcl::primitivep :car))
  (assert-true (com.libgirl.smcl::primitivep :cdr))
  (assert-true (com.libgirl.smcl::primitivep :when))
  (assert-true (com.libgirl.smcl::primitivep :eq))
  (assert-true (com.libgirl.smcl::primitivep :atom))
  (assert-true (com.libgirl.smcl::primitivep :true))
  (assert-true (com.libgirl.smcl::primitivep :none))
  (assert-true (com.libgirl.smcl::primitivep :defun)))

(define-test test-special-primitive-p
  (assert-true (com.libgirl.smcl::special-primitive-p :defun))
  (assert-true (com.libgirl.smcl::special-primitive-p :when))
  (assert-true (com.libgirl.smcl::special-primitive-p :list-quote))
  (assert-true (not (or (com.libgirl.smcl::special-primitive-p :cons)
			(com.libgirl.smcl::special-primitive-p :car)
			(com.libgirl.smcl::special-primitive-p :cdr)
			(com.libgirl.smcl::special-primitive-p :eq)
			(com.libgirl.smcl::special-primitive-p :atom)
			(com.libgirl.smcl::special-primitive-p :true)
			(com.libgirl.smcl::special-primitive-p :none)))))


(define-test test-flate-param
  (assert-equal (list :a) (com.libgirl.smcl::flate-param *symbol-a*))
  (assert-equal (list :a :b) (com.libgirl.smcl::flate-param *list-a-b*))
  (assert-equal (list :a :b :c) (com.libgirl.smcl::flate-param *list-list-c*))
  (assert-equal (list :a :b :c) (com.libgirl.smcl::flate-param *list-a-list*))
  (assert-equal (list :a :b :c :d) (com.libgirl.smcl::flate-param *list-list-list*)))
 
(define-test test-create-procedure-ingredient-list
   (assert-equal (list :a :no-param :arg1 :no-param :arg2)
   		 (com.libgirl.smcl::create-procedure-ingredient-list *symbol-a* :arg1 :arg2))
   (assert-equal (list :a :b :arg1 :no-param :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-a-b* :arg1 :arg2))
   (assert-equal (list :a :b :c :no-param :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-list-c* :arg1 :arg2))
   (assert-equal (list :a :b :c :no-param :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-a-list* :arg1 :arg2))
   (assert-equal (list :a :b :c :d :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-list-list* :arg1 :arg2))
   (assert-equal (list :a :b :c :d :e)
    		(com.libgirl.smcl::create-procedure-ingredient-list *big-list* :arg1 :arg2))
  )

(define-test test-get-parameter-count
  (assert-equal 2 (com.libgirl.smcl::get-parameter-count (list :a :b :c :d :arg2)))
  (assert-equal 1 (com.libgirl.smcl::get-parameter-count (list :a :b :c :no-param :arg2)))
  (assert-equal 0 (com.libgirl.smcl::get-parameter-count (list :a :no-param :arg1 :no-param :arg2))))

(define-test test-find-unprimitve-symbol
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :list-quote))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :cons))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :car))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :cdr))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :eq))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :atom))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :true))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol :none))
  (assert-equal (list :a) (com.libgirl.smcl::find-unprimitive-symbol :a))
  (assert-equal (list :a) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote :a)))
  (assert-equal nil  (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote :cons :car)))
  (assert-equal (list :a) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote (list :list-quote :car :cons) :a)))
  (assert-equal (list :a :b) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote (list :list-quote :a :cons) :b)))
  (assert-equal (list :a :b) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote (list :list-quote :true :a) :b)))
  (assert-equal (list :a :b) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote (list :list-quote :a :b) :atom)))
  (assert-equal (list :a :b :c) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote (list :list-quote :a :b) :c)))

  (assert-equal (list :a) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote :a (list :list-quote :car :cons))))
  (assert-equal (list :a :b) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote :a (list :list-quote :b :cons))))
  (assert-equal (list :a :b) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote :a (list :list-quote :true :b))))
  (assert-equal (list :a :b) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote :none (list :list-quote :a :b))))
  (assert-equal (list :a :b :c) (com.libgirl.smcl::find-unprimitive-symbol (list :list-quote :a (list :list-quote :b :c)))))

;; (define-test test-parse-symbol-to-number
;;   (assert-equal (com.libgirl.smcl::parse-symbol-to-number :list-quote)
;; 		(com.libgirl.smcl::parse-symbol-to-number :list-quote))
;;   (assert-true (not (= (com.libgirl.smcl::parse-symbol-to-number :a)
;; 		       (com.libgirl.smcl::parse-symbol-to-number :b)))))

;; (define-test test-match-params-and-unprimitive-symbol
;;   (assert-equal (list :a)
;; 		(com.libgirl.smcl::match-params-and-unprimitive-symbols (list :name :p1 :a1 :no-param :a2) (list :a)))
;;   (assert-equal (list :b)
;; 		(com.libgirl.smcl::match-params-and-unprimitive-symbols (list :name :p1 :a1 :no-param :a2) (list :a :b)))
;;   (assert-equal (list :a)
;; 		(com.libgirl.smcl::match-params-and-unprimitive-symbols (list :name :p1 :a1 :no-param :a2) (list :a :b :c)))
;;   (assert-equal (list :a)
;; 		(com.libgirl.smcl::match-params-and-unprimitive-symbols (list :name :p1 :a1 :p2 :a2) (list :a)))
;;   (assert-equal (list :b :a)
;; 		(com.libgirl.smcl::match-params-and-unprimitive-symbols (list :name :p1 :a1 :p2 :a2) (list :a :b)))
;;   (assert-equal (list :a :b)
;; 		(com.libgirl.smcl::match-params-and-unprimitive-symbols (list :name :p1 :a1 :p2 :a2) (list :a :b :c)))
;;   )

;;(com.libgirl.smcl::reduce-f :list-quote (get-proc :test-list-quote) *test-procedure-pool*)



(defparameter *test-procedure-pool*
  (let* ((procedure-name-list (list :no-param
				    :one-param
				    :two-params)) 
	 (procedure-param-list (list nil
				     (list :p1)
				     (list :p1 :p2)))
	 (procedure-arg-list (mapcar (lambda (raw-list)
				       (append raw-list
					       (make-list (- com.libgirl.smcl::*arg-size*
							     (length raw-list))
							  :initial-element :0)))
				     (list (list :a1 :a2)
					   (list :a1 :a2)
					   (list :a1 :a2))))
	 (procedure-body-list (list (list :list-quote :xx :yy)
				    (list :list-quote :xx :yy)
				    (list :list-quote :xx :yy)))
	 (cl-user::procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool
						 :init-procedures (mapcar #'list
									  procedure-name-list
									  procedure-param-list
									  procedure-arg-list
									  procedure-body-list))))
    cl-user::procedure-pool
    ))

(defun get-proc (name)
  (format t "~%proc: ~s" name)
  (print (com.libgirl.smcl::procedure-params (com.libgirl.smcl::get-procedure name *test-procedure-pool*)))
  (print (com.libgirl.smcl::procedure-args (com.libgirl.smcl::get-procedure name *test-procedure-pool*)))
  (print (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure name *test-procedure-pool*)))
  (format t "~%")
  (com.libgirl.smcl::get-procedure name *test-procedure-pool*)
  )


(defun test-apply-primitive-f (primitive-name procedure-name)
  (format t "~%~%---~s---" primitive-name)
  (let* ((procedure (get-proc procedure-name))
	 (params (com.libgirl.smcl::procedure-params procedure))
	 (args (com.libgirl.smcl::procedure-args procedure)))
    (com.libgirl.smcl::apply-primitive-f primitive-name
					 (cond ((not params) args)
					       ((= (length params) 1) (append  params (list (car args)))) 
					       ((= (length params) 2) params))
					 args
					 procedure 
					 *test-procedure-pool*)))
(defun test-reduce-f-primitives (body procedure-name)
  (format t "~%~%---~s---" body)
  (let* ((procedure (get-proc procedure-name)))
    (com.libgirl.smcl::reduce-f body procedure *test-procedure-pool*)))



;; (test-apply-primitive-f (list :cdr (list :list-quote (list :list-quote :a :b) :c) :e) :two-params)

 (define-test test-apply-primitive-f
   (assert-equal (list :list-quote :a1 :a2) (test-apply-primitive-f :list-quote :no-param))
   (assert-equal (list :list-quote :p1 :a1) (test-apply-primitive-f :list-quote :one-param))
   (assert-equal (list :list-quote :p1 :p2) (test-apply-primitive-f :list-quote :two-params))
   (assert-equal (list :list-quote :a1 :a2) (test-apply-primitive-f :cons :no-param))
   (assert-equal (list :list-quote :p1 :a1) (test-apply-primitive-f :cons :one-param))
   (assert-equal (list :list-quote :p1 :p2) (test-apply-primitive-f :cons :two-params))
   
   )

(define-test test-reduce-f-primitives
  ;;list-quote
  (assert-equal (list :list-quote :a1 :a2) (test-reduce-f-primitives :list-quote :no-param))
  (assert-equal (list :list-quote :a1 :a2) (test-reduce-f-primitives :list-quote :one-param))
  (assert-equal (list :list-quote :a1 :a2) (test-reduce-f-primitives :list-quote :two-params))
  (assert-equal (list :list-quote :c :a1) (test-reduce-f-primitives (list :list-quote :c) :two-params))
  (assert-equal (list :list-quote :c :d) (test-reduce-f-primitives (list :list-quote :c :d) :two-params))
  (assert-equal (list :list-quote (list :list-quote :c :d) :e)
		(test-reduce-f-primitives (list :list-quote (list :list-quote :c :d) :e)
					  :two-params))
  (assert-equal (list :list-quote :c (list :list-quote :d :e))
		(test-reduce-f-primitives (list :list-quote :c (list :list-quote :d :e))
					  :two-params))
  (assert-equal (list :list-quote (list :list-quote :c :d) (list :list-quote :e :f))
		(test-reduce-f-primitives (list :list-quote (list :list-quote :c :d) (list :list-quote :e :f))
					  :two-params))
  ;;cons
  (assert-equal (list :list-quote :a1 :a2) (test-reduce-f-primitives :cons :no-param))
  (assert-equal (list :list-quote :a1 :a2) (test-reduce-f-primitives :cons :one-param))
  (assert-equal (list :list-quote :a1 :a2) (test-reduce-f-primitives :cons :two-params))
  (assert-equal (list :list-quote :c :a1) (test-reduce-f-primitives (list :cons :c) :two-params))
  (assert-equal (list :list-quote :c :d) (test-reduce-f-primitives (list :cons :c :d) :two-params))
  
  (assert-equal (list :list-quote (list :list-quote :c :d) :e)
		(test-reduce-f-primitives (list :cons (list :list-quote :c :d) :e)
					  :two-params))
  (assert-equal (list :list-quote :c (list :list-quote :d :e))
		(test-reduce-f-primitives (list :cons :c (list :list-quote :d :e))
					  :two-params))
  (assert-equal (list :list-quote (list :list-quote :c :d) (list :list-quote :e :f))
		(test-reduce-f-primitives (list :cons (list :list-quote :c :d) (list :list-quote :e :f))
					  :two-params))
  ;;car
  (assert-equal :a1 (test-reduce-f-primitives :car :no-param))
  (assert-equal :a1 (test-reduce-f-primitives :car :one-param))
  (assert-equal :a1 (test-reduce-f-primitives :car :two-params))
  (assert-equal :c (test-reduce-f-primitives (list :car :c) :two-params))
  (assert-equal :c (test-reduce-f-primitives (list :car :c :d) :two-params))
  
  (assert-equal :c
		(test-reduce-f-primitives (list :car (list :list-quote :c :d) :e)
					  :two-params))
  (assert-equal :c
		(test-reduce-f-primitives (list :car :c (list :list-quote :d :e))
					  :two-params))
  (assert-equal :c 
		(test-reduce-f-primitives (list :car (list :list-quote :c :d) (list :list-quote :e :f))
					  :two-params))
  ;;some perfect form have the params of the procedure in the FIRST place, so we need to take the first one.
  (assert-equal (list :car (list :p1 :c :d) :e)
		(test-reduce-f-primitives (list :car (list :p1 :c :d) :e)
					  :two-params))
  (assert-equal (list :car (list :p1 :c :d) (list :list-quote :e :f))
		(test-reduce-f-primitives (list :car (list :p1 :c :d) (list :list-quote :e :f))
					  :two-params))
  (assert-equal :not-list-quote-not-proc-param
  		(test-reduce-f-primitives (list :car (list :not-list-quote-not-proc-param :c :d) :e)
  					  :two-params))
  (assert-equal :not-list-quote-not-proc-param
  		(test-reduce-f-primitives (list :car (list :not-list-quote-not-proc-param :c :d) (list :list-quote :e :f))
  					  :two-params))
  
  (assert-equal (list :list-quote :a :b)
		(test-reduce-f-primitives (list :car (list :list-quote (list :list-quote :a :b) :c) :e)
					  :two-params))
  
  ;;cdr
  (assert-equal :none (test-reduce-f-primitives :cdr :no-param))
  (assert-equal :none (test-reduce-f-primitives :cdr :one-param))
  (assert-equal :none (test-reduce-f-primitives :cdr :two-params))
  (assert-equal :none (test-reduce-f-primitives (list :cdr :c) :two-params))
  (assert-equal :none (test-reduce-f-primitives (list :cdr :c :d) :two-params))
  
  (assert-equal :d
  		(test-reduce-f-primitives (list :cdr (list :list-quote :c :d) :e)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :cdr :c (list :list-quote :d :e))
  					  :two-params))
  (assert-equal :d
  		(test-reduce-f-primitives (list :cdr (list :list-quote :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal (list :cdr (list :p1 :c :d) :e)
  		(test-reduce-f-primitives (list :cdr (list :p1 :c :d) :e)
  					  :two-params))
  (assert-equal (list :cdr (list :p1 :c :d) (list :list-quote :e :f))
  		(test-reduce-f-primitives (list :cdr (list :p1 :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :cdr (list :not-list-quote-not-proc-param :c :d) :e)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :cdr (list :not-list-quote-not-proc-param :c :d) (list :list-quote :e :f))
  					  :two-params))
  
  (assert-equal :c
  		(test-reduce-f-primitives (list :cdr (list :list-quote (list :list-quote :a :b) :c) :e)
  					  :two-params))
  ;;when
  (assert-equal :a2 (test-reduce-f-primitives :when :no-param))
  (assert-equal :a2 (test-reduce-f-primitives :when :one-param))
  (assert-equal :a2 (test-reduce-f-primitives :when :two-params))
  (assert-equal :a1 (test-reduce-f-primitives (list :when :c) :two-params))
  (assert-equal :d (test-reduce-f-primitives (list :when :c :d) :two-params))
  (assert-equal :e
  		(test-reduce-f-primitives (list :when (list :list-quote :c :d) :e)
  					  :two-params))
  (assert-equal (list :list-quote :d :e)
  		(test-reduce-f-primitives (list :when :c (list :list-quote :d :e))
  					  :two-params))
  (assert-equal (list :list-quote :e :f)
  		(test-reduce-f-primitives (list :when (list :list-quote :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal :e
  		(test-reduce-f-primitives (list :when
						(list :not-list-quote-not-proc-param :c :d)
						:e)
  					  :two-params))
  (assert-equal :not-list-quote-not-proc-param
  		(test-reduce-f-primitives (list :when
						(list :not-list-quote-not-proc-param :c :d)
						(list :not-list-quote-not-proc-param :e :f))
  					  :two-params))
  (assert-equal :e
  		(test-reduce-f-primitives (list :when (list :p1 :c :d) :e)
  					  :two-params))
  (assert-equal (list :list-quote :e :f)
  		(test-reduce-f-primitives (list :when (list :p1 :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal :e
  		(test-reduce-f-primitives (list :when (list :list-quote (list :list-quote :a :b) :c) :e)
  					  :two-params))
  (assert-equal :none
		(test-reduce-f-primitives (list :when :none :a) :two-params))
  ;; eq
  (assert-equal :none (test-reduce-f-primitives :eq :no-param))
  (assert-equal :none (test-reduce-f-primitives :eq :one-param))
  (assert-equal :none (test-reduce-f-primitives :eq :two-params))
  (assert-equal :none (test-reduce-f-primitives (list :eq :c) :two-params))
  (assert-equal :true (test-reduce-f-primitives (list :eq :c :c) :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :eq (list :list-quote :c :d) :e)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :eq :c (list :list-quote :d :e))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :eq (list :list-quote :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :eq (list :list-quote :c :d) (list :list-quote :c :d))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :eq (list :p1 :c :d) :e)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :eq (list :p1 :c :d) (list :p1 :c :d))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :eq
						(list :not-list-quote-not-proc-param :c :d)
						:e)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :eq
						(list :not-list-quote-not-proc-param :c :d)
						:not-list-quote-not-proc-param)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :eq
						(list :not-list-quote-not-proc-param :c :d)
						(list :not-list-quote-not-proc-param :e :f))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :eq (list :list-quote (list :list-quote :a :b) :c) :e)
  					  :two-params))
  ;; atom
  (assert-equal :true (test-reduce-f-primitives :atom :no-param))
  (assert-equal :true (test-reduce-f-primitives :atom :one-param))
  (assert-equal :true (test-reduce-f-primitives :atom :two-params))
  (assert-equal :true (test-reduce-f-primitives (list :atom :c) :two-params))
  (assert-equal :true (test-reduce-f-primitives (list :atom :c :c) :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :atom (list :list-quote :c :d) :e)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :atom :c (list :list-quote :d :e))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :atom (list :list-quote :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :atom (list :list-quote :c :d) (list :list-quote :c :d))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :atom (list :p1 :c :d) :e)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :atom (list :p1 :c :d) (list :p1 :c :d))
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :atom
						(list :not-list-quote-not-proc-param :c :d)
						:e)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :atom
						(list :not-list-quote-not-proc-param :c :d)
						:not-list-quote-not-proc-param)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :atom
						(list :not-list-quote-not-proc-param :c :d)
						(list :not-list-quote-not-proc-param :e :f))
  					  :two-params))
  
  
 

  )




