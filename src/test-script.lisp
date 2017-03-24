(defun my-print (string)
  (format t "~%  (╯*≥▽≤)╯ ═╩═══════╩═ ~S ~%" string))

(my-print "qLoad iterate")
(ql:quickload "iterate")

(my-print "qLoad smcl")
(ql:quickload "smcl")

(my-print "Load lisp-unit")
;;lisp-unit
;;https://github.com/OdonataResearchLLC/lisp-unit/wiki
(load "../t/lisp-unit")



(defpackage test-script
  (:use :cl :iterate :lisp-unit :com.libgirl.smcl))
(in-package :test-script)
(setq *print-failures* t)
(load "../t/test-primitives.lisp")
(RUN-TESTS :ALL)
