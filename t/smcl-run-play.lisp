
(in-package :cl-user)

(defpackage com.libgirl.smcl-run-play
  (:use :cl :com.libgirl.smcl))
(in-package :com.libgirl.smcl-run-play)

(smcl-thread-run '((:x nil
		    (:0
		     :0)
		    (:list-quote
		     (:list-quote (:y :a :b)
		      :y)
		     :c))
		   (:y (:p1 :p2)
		    (:yp
		     :yp)
		    (:list-quote :p1 :p2))
		   (:a (:p1 :p2)
		    (:a1
		     :0)
		    (:list-quote :0 (:y :d)))
		   (:c nil
		    (:0
		     :0)
		    :e)
		   (:e (:p1)
		    (:0
		     :e2)
		    (:p1 (:list-quote :p1 :1)))
		   (:f (:x)
		    (:f1
		     :f2)
		    (:e (:y :x)))))

