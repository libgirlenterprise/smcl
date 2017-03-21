;; use lisp-unit

(REMOVE-TESTS :ALL)
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
  (assert-true (primitivep 'list-quote))
  (assert-true (primitivep 'cons))
  (assert-true (primitivep 'car))
  (assert-true (primitivep 'cdr))
  (assert-true (primitivep 'when))
  (assert-true (primitivep 'eq))
  (assert-true (primitivep 'atom))
  (assert-true (primitivep 'true))
  (assert-true (primitivep 'none))
  (assert-true (primitivep 'defun)))

(define-test test-special-primitive-p
  (assert-true (special-primitive-p 'defun))
  (assert-true (special-primitive-p 'when))
  (assert-true (not (or (special-primitive-p 'list-quote)
			(special-primitive-p 'cons)
			(special-primitive-p 'car)
			(special-primitive-p 'cdr)
			(special-primitive-p 'eq)
			(special-primitive-p 'atom)
			(special-primitive-p 'true)
			(special-primitive-p 'none)))))


(define-test test-flate-param
  (assert-equal (list 'a) (flate-param *symbol-a*))
  (assert-equal (list 'a 'b) (flate-param *list-a-b*))
  (assert-equal (list 'a 'b 'c) (flate-param *list-list-c*))
  (assert-equal (list 'a 'b 'c) (flate-param *list-a-list*))
  (assert-equal (list 'a 'b 'c 'd) (flate-param *list-list-list*)))
 
(define-test test-create-procedure-ingredient-list
  (assert-equal (list 'a :none 'arg1 :none 'arg2) (create-procedure-ingredient-list *symbol-a* 'arg1 'arg2))
  (assert-equal (list 'a 'b 'arg1 :none 'arg2) (create-procedure-ingredient-list *list-a-b* 'arg1 'arg2))
  (assert-equal (list 'a 'b 'c :none 'arg2) (create-procedure-ingredient-list *list-list-c* 'arg1 'arg2))
  (assert-equal (list 'a 'b 'c :none 'arg2) (create-procedure-ingredient-list *list-a-list* 'arg1 'arg2))
  (assert-equal (list 'a 'b 'c 'd 'arg2) (create-procedure-ingredient-list *list-list-list* 'arg1 'arg2))
  (assert-equal (list 'a 'b 'c 'd 'e) (create-procedure-ingredient-list *big-list* 'arg1 'arg2))
  )
