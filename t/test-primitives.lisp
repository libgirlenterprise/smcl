;; use lisp-unit

(REMOVE-TESTS :ALL)
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
			(special-primitive-p 'tr)
			(special-primitive-p 'ni)))))
 

