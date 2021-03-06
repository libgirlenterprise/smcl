;; use lisp-unit

(defmacro make-test-thread (thread-form)
  `(progn (setf com.libgirl.smcl::*smcl-thread*
		(sb-thread:make-thread (lambda (standard-output)
					 (let* ((*standard-output* standard-output)
						(test-form-list ,thread-form))
					   (setf com.libgirl.smcl::*hand-to-api-p* t)
					   (sb-thread:signal-semaphore com.libgirl.smcl::*api-semaphore* 1)
					   (sb-thread:return-from-thread test-form-list)))
				       :arguments (list *standard-output*)))
	  (sb-thread:thread-yield)
					;	  (print (sb-thread:semaphore-count com.libgirl.smcl::*api-semaphore*))
	  (sb-thread:wait-on-semaphore com.libgirl.smcl::*api-semaphore*)))

(defun run-smcl-steps-and-join-thread()
  (let ((test-form-list))
    (loop until (setf test-form-list
		      (sb-thread:join-thread com.libgirl.smcl::*smcl-thread*
					     :default nil
					     :timeout 0.001))
	  do (smcl-run-steps))
    (mapcar #'eval test-form-list)))


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
   (assert-equal (list :a nil :arg1 nil :arg2)
   		 (com.libgirl.smcl::create-procedure-ingredient-list *symbol-a* (list :arg1 :arg2)))
   (assert-equal (list :a :b :arg1 nil :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-a-b* (list :arg1 :arg2)))
   (assert-equal (list :a :b :c nil :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-list-c* (list :arg1 :arg2)))
   (assert-equal (list :a :b :c nil :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-a-list* (list :arg1 :arg2)))
   (assert-equal (list :a :b :c :d :arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-list-list* (list :arg1 :arg2)))
   (assert-equal (list :a :b :c :d :e)
    		(com.libgirl.smcl::create-procedure-ingredient-list *big-list* (list :arg1 :arg2)))
  )

(define-test test-get-parameter-count
  (assert-equal 2 (com.libgirl.smcl::get-parameter-count (list :a :b :c :d :arg2)))
  (assert-equal 1 (com.libgirl.smcl::get-parameter-count (list :a :b :c nil :arg2)))
  (assert-equal 0 (com.libgirl.smcl::get-parameter-count (list :a nil :arg1 nil :arg2))))

(defun find-unprimitive-symbol (body)
  (com.libgirl.smcl::find-unprimitive-symbol body (list :p1 :p2)))

(define-test test-find-unprimitve-symbol
  (assert-equal nil (find-unprimitive-symbol :list-quote))
  (assert-equal nil (find-unprimitive-symbol :cons))
  (assert-equal nil (find-unprimitive-symbol :car))
  (assert-equal nil (find-unprimitive-symbol :cdr))
  (assert-equal nil (find-unprimitive-symbol :eq))
  (assert-equal nil (find-unprimitive-symbol :atom))
  (assert-equal nil (find-unprimitive-symbol :true))
  (assert-equal nil (find-unprimitive-symbol :none))
  (assert-equal (list :a) (find-unprimitive-symbol :a))
  (assert-equal (list :a) (find-unprimitive-symbol (list :list-quote :a)))
  (assert-equal nil  (find-unprimitive-symbol (list :list-quote :cons :car)))
  (assert-equal (list :a) (find-unprimitive-symbol (list :list-quote (list :list-quote :car :cons) :a)))
  (assert-equal (list :a :b) (find-unprimitive-symbol (list :list-quote (list :list-quote :a :cons) :b)))
  (assert-equal (list :a :b) (find-unprimitive-symbol (list :list-quote (list :list-quote :true :a) :b)))
  (assert-equal (list :a :b) (find-unprimitive-symbol (list :list-quote (list :list-quote :a :b) :atom)))
  (assert-equal (list :a :b :c) (find-unprimitive-symbol (list :list-quote (list :list-quote :a :b) :c)))

  (assert-equal (list :a) (find-unprimitive-symbol (list :list-quote :a (list :list-quote :car :cons))))
  (assert-equal (list :a :b) (find-unprimitive-symbol (list :list-quote :a (list :list-quote :b :cons))))
  (assert-equal (list :a :b) (find-unprimitive-symbol (list :list-quote :a (list :list-quote :true :b))))
  (assert-equal (list :a :b) (find-unprimitive-symbol (list :list-quote :none (list :list-quote :a :b))))
  (assert-equal (list :a :b :c) (find-unprimitive-symbol (list :list-quote :a (list :list-quote :b :c)))))

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
  (progn
    (let ((cl-user::procedure-pool nil))
      (make-test-thread (let* ((procedure-name-list (list :no-param
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
			       (procedure-pool-instance (make-instance 'com.libgirl.smcl::procedure-pool
								       :init-procedures (mapcar #'list
												procedure-name-list
												procedure-param-list
												procedure-arg-list
												procedure-body-list))))
			  
			  (setf cl-user::procedure-pool procedure-pool-instance)
			  '(nil)))
      (run-smcl-steps-and-join-thread)
      cl-user::procedure-pool)))
    



(defun get-proc (name)
  (format t "~%procedure~%")
  (format t "  name : ~s~%" name)
  (format t "  params: ")
  (princ (com.libgirl.smcl::procedure-params (com.libgirl.smcl::get-procedure name *test-procedure-pool*)))
  (format t "~%  arge : ")
  (princ (com.libgirl.smcl::procedure-args (com.libgirl.smcl::get-procedure name *test-procedure-pool*)))
  (format t "~%  body : ")
  (princ (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure name *test-procedure-pool*)))
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
  (format t "~s" *test-procedure-pool*)
  (let ((result nil))
    (make-test-thread (let* ((procedure (get-proc procedure-name)))
			(setf result (com.libgirl.smcl::reduce-f body procedure *test-procedure-pool*))
			'(nil)))
    (run-smcl-steps-and-join-thread)
    result))



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

  ;; true
  (assert-equal :true (test-reduce-f-primitives :true :no-param))
  (assert-equal :true (test-reduce-f-primitives :true :one-param))
  (assert-equal :true (test-reduce-f-primitives :true :two-params))
  (assert-equal :true (test-reduce-f-primitives (list :true :c) :two-params))
  (assert-equal :true (test-reduce-f-primitives (list :true :c :c) :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true (list :list-quote :c :d) :e)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true :c (list :list-quote :d :e))
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true (list :list-quote :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true (list :list-quote :c :d) (list :list-quote :c :d))
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true (list :p1 :c :d) :e)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true (list :p1 :c :d) (list :p1 :c :d))
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true
						(list :not-list-quote-not-proc-param :c :d)
						:e)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true
						(list :not-list-quote-not-proc-param :c :d)
						:not-list-quote-not-proc-param)
  					  :two-params))
  (assert-equal :true
  		(test-reduce-f-primitives (list :true
						(list :not-list-quote-not-proc-param :c :d)
						(list :not-list-quote-not-proc-param :e :f))
  					  :two-params))
  
  ;; none
  (assert-equal :none (test-reduce-f-primitives :none :no-param))
  (assert-equal :none (test-reduce-f-primitives :none :one-param))
  (assert-equal :none (test-reduce-f-primitives :none :two-params))
  (assert-equal :none (test-reduce-f-primitives (list :none :c) :two-params))
  (assert-equal :none (test-reduce-f-primitives (list :none :c :c) :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none (list :list-quote :c :d) :e)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none :c (list :list-quote :d :e))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none (list :list-quote :c :d) (list :list-quote :e :f))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none (list :list-quote :c :d) (list :list-quote :c :d))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none (list :p1 :c :d) :e)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none (list :p1 :c :d) (list :p1 :c :d))
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none
						(list :not-list-quote-not-proc-param :c :d)
						:e)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none
						(list :not-list-quote-not-proc-param :c :d)
						:not-list-quote-not-proc-param)
  					  :two-params))
  (assert-equal :none
  		(test-reduce-f-primitives (list :none
						(list :not-list-quote-not-proc-param :c :d)
						(list :not-list-quote-not-proc-param :e :f))
  					  :two-params))  

  )

(defun test-reduce-f-defun (body procedure-name)
  (format t "~%~%---~s---" body)
  (let ((result nil))
    (make-test-thread (let* ((procedure-name-list (list :no-param
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
			     (defun-procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool
								  :init-procedures (mapcar #'list
											   procedure-name-list
											   procedure-param-list
											   procedure-arg-list
											   procedure-body-list)))
			     (defun-procedure
			       (progn
				 (format t "~%procedure~%")
				 (format t "  name : ~s~%" procedure-name)
				 (format t "  params: ")
				 (princ (com.libgirl.smcl::procedure-params (com.libgirl.smcl::get-procedure procedure-name defun-procedure-pool)))
				 (format t "~%  arge : ")
				 (princ (com.libgirl.smcl::procedure-args (com.libgirl.smcl::get-procedure procedure-name defun-procedure-pool)))
				 (format t "~%  body : ")
				 (princ (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure procedure-name defun-procedure-pool)))
				 (format t "~%")
				 (com.libgirl.smcl::get-procedure procedure-name defun-procedure-pool))))
			(setf result (com.libgirl.smcl::reduce-f body defun-procedure defun-procedure-pool))
			'(nil)))
    (run-smcl-steps-and-join-thread)
    result))

(test-reduce-f-defun (list :defun :p1) :one-param)
 
  
(define-test test-defun
  ;; defun atom, list-quote list,non-list-quote list with no-param
  (assert-equal :a2
		(test-reduce-f-defun :defun :no-param))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun :a) :no-param))
  (assert-equal :b
		(test-reduce-f-defun (list :defun :a :b) :no-param))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun (list :list-quote :a :b))
				     :no-param))
  (assert-equal :a1			;this a1 is from defun-procedure's default arg
		(test-reduce-f-defun (list :defun (list :list-quote :a :b) :c)
				     :no-param))
  (assert-equal (list :list-quote :b :c)
		(test-reduce-f-defun (list :defun :a (list :list-quote :b :c))
				     :no-param))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun (list :not-list-quote :a :b))
				     :no-param))
  (assert-equal :c			
		(test-reduce-f-defun (list :defun (list :not-list-quote :a :b) :c)
				     :no-param))
  (assert-equal :not-list-quote
		(test-reduce-f-defun (list :defun :a (list :not-list-quote :b :c))
				     :no-param))
  ;; defun atom, list-quote list,non-list-quote list with one-param
  (assert-equal :a2
		(test-reduce-f-defun :defun :one-param))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun :a) :one-param))
  (assert-equal :b
		(test-reduce-f-defun (list :defun :a :b) :one-param))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun (list :list-quote :a :b))
				     :one-param))
  (assert-equal :a1			;this a1 is from defun-procedure's default arg
		(test-reduce-f-defun (list :defun (list :list-quote :a :b) :c)
				     :one-param))
  (assert-equal (list :list-quote :b :c)
		(test-reduce-f-defun (list :defun :a (list :list-quote :b :c))
				     :one-param))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun (list :not-list-quote :a :b))
				     :one-param))
  (assert-equal :c			
		(test-reduce-f-defun (list :defun (list :not-list-quote :a :b) :c)
				     :one-param))
  (assert-equal :not-list-quote
		(test-reduce-f-defun (list :defun :a (list :not-list-quote :b :c))
				     :one-param))
  ;; defun atom, list-quote list,non-list-quote list with two-params
  (assert-equal :a2
		(test-reduce-f-defun :defun :two-params))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun :a) :two-params))
  (assert-equal :b
		(test-reduce-f-defun (list :defun :a :b) :two-params))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun (list :a :p1 :b))
				     :two-params))
  (assert-equal :a1			;this a1 is from defun-procedure's default arg
		(test-reduce-f-defun (list :defun (list :list-quote :a :b) :c)
				     :two-params))
  (assert-equal (list :list-quote :b :c)
		(test-reduce-f-defun (list :defun :a (list :list-quote :b :c))
				     :two-params))
  (assert-equal :a1
		(test-reduce-f-defun (list :defun (list :not-list-quote :a :b))
				     :two-params))
  (assert-equal :c			
		(test-reduce-f-defun (list :defun (list :not-list-quote :a :b) :c)
				     :two-params))
  (assert-equal :not-list-quote
		(test-reduce-f-defun (list :defun :a (list :not-list-quote :b :c))
				     :two-params))
  ;;  defun contain p
  (assert-equal (list :defun :p1 :a1)
  		(test-reduce-f-defun (list :defun :p1) :one-param))
  (assert-equal (list :defun :p1 :a)
  		(test-reduce-f-defun (list :defun :p1 :a) :one-param))
  (assert-equal (list :defun :a :p1)
  		(test-reduce-f-defun (list :defun :a :p1) :one-param))
  (assert-equal (list :defun :a :p1)
  		(test-reduce-f-defun (list :defun (list :a :b) :p1) :one-param))
  
  (assert-equal (list :defun :p1 (list :a :b))
  		(test-reduce-f-defun (list :defun :p1 (list :a :b)) :one-param))

  
  (assert-equal (list :defun (list :list-quote :a :p1) :a1)
  		(test-reduce-f-defun (list :defun (list :list-quote :a :p1))
  				     :one-param))
  (assert-equal (list :defun (list :list-quote :p1 :a) :a1)
  		(test-reduce-f-defun (list :defun (list :list-quote :p1 :a))
  				     :one-param))
  (assert-equal (list :defun (list :list-quote :a :b) :p1)	
   		(test-reduce-f-defun (list :defun (list :list-quote :a :b) :p1)
   				     :one-param))
  (assert-equal (list :defun :p1 (list :list-quote :b :c))
  		(test-reduce-f-defun (list :defun :p1 (list :list-quote :b :c))
  				     :one-param))
  (assert-equal (list :defun (list :p1 :a :b) :a1)
   		(test-reduce-f-defun (list :defun (list :p1 :a :b))
   				     :one-param))
    (assert-equal (list :defun :not-list-quote :p1)			
   		(test-reduce-f-defun (list :defun (list :not-list-quote :a :b) :p1)
   				     :one-param))
  (assert-equal (list :defun :p1 (list :not-list-quote :b :c))
   		(test-reduce-f-defun (list :defun :p1 (list :not-list-quote :b :c))
   				     :one-param))


  ;; :a1
  (assert-equal :a1
   		(test-reduce-f-defun (list :defun (list :a :p1 :b))
   				     :one-param))
  
  (assert-equal :a1
   		(test-reduce-f-defun (list :defun (list :a :b :p1))
   				     :one-param))


  
  (assert-equal (list :defun :a  (list :p1 :b :c))
   		(test-reduce-f-defun (list :defun :a (list :p1 :b :c))
   				     :one-param))
  (assert-equal :a1
   		(test-reduce-f-defun (list :defun (list :a :p1 :b))
   				     :one-param))
  
  (assert-equal (list :defun :a (list :b :c :p1))
   		(test-reduce-f-defun (list :defun :a (list :b :c :p1))
   				     :one-param))

 )








