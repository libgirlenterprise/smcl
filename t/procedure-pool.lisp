(in-package :cl-user)

(defpackage com.libgirl.smcl-test.procedure-pool
  (:use :cl :com.libgirl.smcl :prove :com.libgirl.smcl-test)
  (:import-from :com.libgirl.smcl-test :*test-export-to-file-filepath*))
(in-package :com.libgirl.smcl-test.procedure-pool)

(defparameter *nil-function* (lambda () ())) ;TODO: make it a constant

(defparameter *simple-case-data-size* 3)

(defparameter *simple-case-data* (list (list :x :y :z)
				       (make-list *simple-case-data-size*)
				       (make-list *simple-case-data-size*
						  :initial-element (make-list com.libgirl.smcl::*arg-size*
									      :initial-element '0))
				       (list :y :z :v)))

(defparameter *non-export-symbol-list* '("*MAX-PARAM-SIZE*"
					 "*ARG-SIZE*"
					 "*USER-INPUT-FUNCTION*"
					 "MAKE-USER-INPUT-FUNCTION"
					 "PROCEDURE"
					 "PROCEDURE-PARAMS"
					 "PROCEDURE-ARGS"
					 "PROCEDURE-BODY"
					 "PROCEDURE-POOL"
					 "PROCEDURES"
					 "GET-PROCEDURE"
					 "REDUCE-F"
					 "INVOKE-F"
					 "SET-PROCEDURE"
					 "EXPORT-TO-FILE"))

(defparameter *subtest-number-list* (list (+ 1 (* 3 (length *non-export-symbol-list*)))
					  4
					  6
					  6
					  12
					  16))

(setf com.libgirl.smcl::*user-input-function*
      *nil-function*)

(defun run-program-for-make-user-input-function (input-stream)
  (sb-ext:run-program "/usr/local/bin/sbcl"
		      '("--noinform" "--load" "make-user-input-function.lisp")
		      :output nil
		      :input input-stream
		      :wait nil))

(plan (length *subtest-number-list*))

(subtest "test export or non-export of com.libgirl.smcl"
  (plan (first *subtest-number-list*))
  (mapcar #'(lambda (symbol)
	      (ok (not (find-symbol symbol)))
	      (ok (find-symbol symbol 'com.libgirl.smcl))
	      (is (second (multiple-value-list (find-symbol symbol
							    'com.libgirl.smcl)))
		  :internal))
	  *non-export-symbol-list*)
  (ok (find-symbol "SMCL-RUN"))
  (finalize))


(subtest "null cases"
  (plan (second *subtest-number-list*))
  (let* ((empty-procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool))
	 (cl-user::procedures (slot-value empty-procedure-pool 'com.libgirl.smcl::procedures)))
    (is-type empty-procedure-pool 'com.libgirl.smcl::procedure-pool)
    (is-type cl-user::procedures 'hash-table)
    (is (hash-table-count cl-user::procedures) 0)
    (is (com.libgirl.smcl::reduce-f :x
				    (com.libgirl.smcl::make-procedure)
				    empty-procedure-pool)
	:x))
  (finalize))


(defun test-simple-case (subtest-name subtest-number cl-user::procedure-pool)
  (subtest subtest-name
    (plan subtest-number)
    (is (com.libgirl.smcl::get-procedure :v cl-user::procedure-pool)
	nil)
    (is (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :x
												       cl-user::procedure-pool))
				    (com.libgirl.smcl::get-procedure :x cl-user::procedure-pool)
				    cl-user::procedure-pool)
	:v)
    (is (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :x
									   cl-user::procedure-pool))
	:y)
    (is (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :y
									   cl-user::procedure-pool))
	:v)
    (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :x
												   cl-user::procedure-pool))
				(com.libgirl.smcl::get-procedure :x cl-user::procedure-pool)
				cl-user::procedure-pool
				:set-procedure-new-body-p t)
    (is (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :x
									   cl-user::procedure-pool))
	:v)
    (is (com.libgirl.smcl::get-procedure :v cl-user::procedure-pool)
	nil)
    (finalize)))


(let ((procedure-pool-1 (make-instance 'com.libgirl.smcl::procedure-pool)))
  (apply #'mapcar
	 (cons #'com.libgirl.smcl::set-procedure
	       (append (copy-tree *simple-case-data*)
		       (list (make-list *simple-case-data-size* :initial-element procedure-pool-1)))))
  (test-simple-case "test simple case with set-procedure"
		    (third *subtest-number-list*)
		    procedure-pool-1))
(test-simple-case "test simple case wiht initialze-instance method"
		  (fourth *subtest-number-list*)
		  (make-instance 'com.libgirl.smcl::procedure-pool
				 :init-procedures (apply #'mapcar
							 (cons #'list
							       *simple-case-data*))))

(defun test-multiple-name-body-pairs (procedure-name-list procedure-body-expected-list cl-user::procedure-pool)
  (mapcar (lambda (procedure-name procedure-body-expected)
	    (is (let ((procedure-got (com.libgirl.smcl::get-procedure procedure-name
						    cl-user::procedure-pool)))
		  (when procedure-got
		    (com.libgirl.smcl::procedure-body procedure-got)))
		procedure-body-expected
		:test #'equalp))
	  procedure-name-list
	  procedure-body-expected-list))

(subtest "test normal case but without parameter"
  (plan (fifth *subtest-number-list*))
  (let ((procedure-name-list (list :x :y :v :z :a))
	(procedure-body-list (list (list :y :z)
				   (list :v :u)
				   :w
				   (list :a :b)
				   :c)))
    (let* ((cl-user::procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool
						   :init-procedures (mapcar #'list
									    procedure-name-list
									    (make-list 5)
									    (make-list 5
										       :initial-element (make-list com.libgirl.smcl::*arg-size*
														   :initial-element '0))
									    procedure-body-list)))
	   (procedure-x (com.libgirl.smcl::get-procedure :x
							 cl-user::procedure-pool)))
      ;;after initialization, name-body corresponding should be still the same
      (test-multiple-name-body-pairs procedure-name-list
				     procedure-body-list
				     cl-user::procedure-pool)
      (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body procedure-x)
				  procedure-x
				  cl-user::procedure-pool
				  :set-procedure-new-body-p t)
      (test-multiple-name-body-pairs (append procedure-name-list
					     (list :u :b))
				     (list :w :w :w :c :c nil nil)
				     cl-user::procedure-pool)))
  (finalize))


(let* ((procedure-name-list (list :x :y :z :b :a :0 :c))
       (procedure-param-list (list nil
				   (list :p1 :p2)
				   (list :p1 :p2)
				   (list :p1 :p2)
				   (list :p1)
				   (list :p1 :p2)
				   nil))
       (procedure-arg-list (mapcar (lambda (raw-list)
				     (append raw-list
					     (make-list (- com.libgirl.smcl::*arg-size*
							   (length raw-list))
							:initial-element :0)))
				   (list (list :x1 :x2)
					 nil
					 (list :z1 :z2)
					 (list :b1 :b2)
					 (list :aa)
					 nil
					 nil)))
       (procedure-body-list (list (list :y
					(list :z :y :v)
					(list :a
					      (list :b :c :k)))
				  (list :p1 :p2 :k)
				  (list :p1
					(list :b :p2)
					:c)
				  :p2
				  :p1
				  :p2
				  :d))
       (cl-user::procedure-pool (make-instance 'com.libgirl.smcl::procedure-pool
					       :init-procedures (mapcar #'list
									procedure-name-list
									procedure-param-list
									procedure-arg-list
									procedure-body-list))))
  
  (subtest "test normal case with parameters"
    (plan (sixth *subtest-number-list*))
    (test-multiple-name-body-pairs procedure-name-list ; first test the status unchanged before reduction
				   procedure-body-list
				   cl-user::procedure-pool)
    (is (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :x
												       cl-user::procedure-pool))
				    (com.libgirl.smcl::get-procedure :x
								     cl-user::procedure-pool)
				    cl-user::procedure-pool)
	:x1)

    (test-multiple-name-body-pairs procedure-name-list
				   (let ((expected (copy-tree procedure-body-list)))
				     (setf (first expected) (list :y :x1 :k)
					   (third expected) (list :p1 :z1 :d))
				     expected)
				   cl-user::procedure-pool)
    (com.libgirl.smcl::reduce-f (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :x
												   cl-user::procedure-pool))
				(com.libgirl.smcl::get-procedure :x
								 cl-user::procedure-pool)
				cl-user::procedure-pool
				:set-procedure-new-body-p t)
    (is (com.libgirl.smcl::procedure-body (com.libgirl.smcl::get-procedure :x
									   cl-user::procedure-pool))
	:x1)
    (finalize)))

  ;(subtest "test export-to-file"
					;(plan (seventh *subtest-number-list*))
  ;; (com.libgirl.smcl::export-to-file (format nil
  ;; 					    "~a"
  ;; 					    (read))
  ;; 				    cl-user::procedure-pool))
   ; (finalize)))
    
(finalize)

