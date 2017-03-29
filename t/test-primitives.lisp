;; use lisp-unit


(defparameter *symbol-a* 'a)
(defparameter *list-a* (list 'a))
(defparameter *list-a-b* (list 'list-quote 'a 'b))
(defparameter *list-list-c* (list 'list-quote (list 'list-quote 'a 'b) 'c))
(defparameter *list-a-list* (list 'list-quote 'a (list 'list-quote 'b 'c)))
(defparameter *list-list-list* (list 'list-quote (list 'list-quote 'a 'b) (list 'list-quote 'c 'd)))
(defparameter *big-list* (list 'list-quote
			       (list 'list-quote
				     (list 'list-quote 'a 'b)
				     'c)
			       (list 'list-quote 'd 'e)))

(define-test test-primitivep
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::list-quote))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::cons))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::car))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::cdr))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::when))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::eq))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::atom))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::true))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::none))
  (assert-true (com.libgirl.smcl::primitivep 'com.libgirl.smcl::defun)))

(define-test test-special-primitive-p
  (assert-true (com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::defun))
  (assert-true (com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::when))
  (assert-true (not (or (com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::list-quote)
			(com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::cons)
			(com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::car)
			(com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::cdr)
			(com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::eq)
			(com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::atom)
			(com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::true)
			(com.libgirl.smcl::special-primitive-p 'com.libgirl.smcl::none)))))


(define-test test-flate-param
  (assert-equal (list 'a) (com.libgirl.smcl::flate-param *symbol-a*))
  (assert-equal (list 'a 'b) (com.libgirl.smcl::flate-param *list-a-b*))
  (assert-equal (list 'a 'b 'c) (com.libgirl.smcl::flate-param *list-list-c*))
  (assert-equal (list 'a 'b 'c) (com.libgirl.smcl::flate-param *list-a-list*))
  (assert-equal (list 'a 'b 'c 'd) (com.libgirl.smcl::flate-param *list-list-list*)))
 
(define-test test-create-procedure-ingredient-list
   (assert-equal (list 'a :none 'arg1 :none 'arg2)
   		 (com.libgirl.smcl::create-procedure-ingredient-list *symbol-a* 'arg1 'arg2))
   (assert-equal (list 'a 'b 'arg1 :none 'arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-a-b* 'arg1 'arg2))
   (assert-equal (list 'a 'b 'c :none 'arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-list-c* 'arg1 'arg2))
   (assert-equal (list 'a 'b 'c :none 'arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-a-list* 'arg1 'arg2))
   (assert-equal (list 'a 'b 'c 'd 'arg2)
   		(com.libgirl.smcl::create-procedure-ingredient-list *list-list-list* 'arg1 'arg2))
   (assert-equal (list 'a 'b 'c 'd 'e)
    		(com.libgirl.smcl::create-procedure-ingredient-list *big-list* 'arg1 'arg2))
  )

(define-test test-get-parameter-count
  (assert-equal 2 (com.libgirl.smcl::get-parameter-count (list 'a 'b 'c 'd 'arg2)))
  (assert-equal 1 (com.libgirl.smcl::get-parameter-count (list 'a 'b 'c :none 'arg2)))
  (assert-equal 0 (com.libgirl.smcl::get-parameter-count (list 'a :none 'arg1 :none 'arg2))))

(define-test test-find-unprimitve-symbol
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::list-quote))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::cons))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::car))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::cdr))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::eq))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::atom))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::true))
  (assert-equal nil (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::none))
  (assert-equal com.libgirl.smcl::(list 'a) (com.libgirl.smcl::find-unprimitive-symbol 'com.libgirl.smcl::a))
  (assert-equal com.libgirl.smcl::(list 'a) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote 'a)))
  (assert-equal nil  (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote 'cons 'car)))
  (assert-equal com.libgirl.smcl::(list 'a) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote (list 'list-quote 'car 'cons) 'a)))
  (assert-equal com.libgirl.smcl::(list 'a 'b) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote (list 'list-quote 'a 'cons) 'b)))
  (assert-equal com.libgirl.smcl::(list 'a 'b) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote (list 'list-quote 'true 'a) 'b)))
  (assert-equal com.libgirl.smcl::(list 'a 'b) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote (list 'list-quote 'a 'b) 'atom)))
  (assert-equal com.libgirl.smcl::(list 'a 'b 'c) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote (list 'list-quote 'a 'b) 'c)))

    (assert-equal com.libgirl.smcl::(list 'a) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote 'a (list 'list-quote 'car 'cons))))
  (assert-equal com.libgirl.smcl::(list 'a 'b) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote 'a (list 'list-quote 'b 'cons))))
  (assert-equal com.libgirl.smcl::(list 'a 'b) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote 'a (list 'list-quote 'true 'b))))
  (assert-equal com.libgirl.smcl::(list 'a 'b) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote 'none (list 'list-quote 'a 'b))))
  (assert-equal com.libgirl.smcl::(list 'a 'b 'c) (com.libgirl.smcl::find-unprimitive-symbol com.libgirl.smcl::(list 'list-quote 'a (list 'list-quote 'b 'c)))))

(define-test test-parse-name-to-number
  (assert-equal (com.libgirl.smcl::parse-name-to-number 'com.libgirl.smcl::list-quote)
		(com.libgirl.smcl::parse-name-to-number 'com.libgirl.smcl::list-quote))
  (assert-true (not (= (com.libgirl.smcl::parse-name-to-number 'com.libgirl.smcl::a)
		       (com.libgirl.smcl::parse-name-to-number 'com.libgirl.smcl::b)))))
