# Self-Modifying Common Lisp
## Introduction
[Self-modifying Common Lisp - Dialect Design & Implementation](https://speakerdeck.com/austintinglibgirl/self-modifying-common-lisp)
## Usage
1. Quickload it in the project file.
    ```
    CL-USER> (ql:quickload "smcl")
    To load "smcl":
      Load 1 ASDF system:
        smcl
    ; Loading "smcl"

    ("smcl")
    CL-USER> 
    ```
2. Put the source code to run it. (You can change the source code too.)
    ```
    CL-USER> (com.libgirl.smcl:smcl-thread-run '((:x nil
                  (:0
                   :0)
                  (:list-quote
                   (:car (:y :a :b)
                     :y)
                   :c))
                 (:y (:p1 :p2)
                  (:yp
                   :yp)
                  (:when :p1 :p2))
                 (:a (:p1 :p2)
                  (:a1
                   :0)
                  (:list-quote :true (:y :d)))
                 (:c nil
                  (:0
                   :0)
                  :e)
                 (:e (:p1)
                  (:0
                   :e2)
                  (:p1 (:list-quote :p1 :none)))
                 (:f (:x)
                  (:f1
                   (:list-quote (:defun :f :g)))
                  (:e (:y :x)))
                 (:8 (:p1)
                  (:3
                   :9)
                  (:when (:when :w) (:cdr :p1)))
                 (:z (:p1 :p2)
                  ((:list-quote :defun :8)
                   (:defun :f (:list-quote (:list-quote :3 (:list-quote :car :cdr)))))
                  (:eq :p1 (:p2 (:e :p1) :1)))))
    0
    CL-USER>
    ```
3. Get the character now.
    ```
    CL-USER> (com.libgirl.smcl:smcl-get-char)
    #\0
    CL-USER>
    ```
4. Run steps. (Default step count is 1)
    ```
    CL-USER> (com.libgirl.smcl:smcl-run-steps)
    T
    CL-USER>
    ```
5. Repeat 3. and 4. to check the value after self-modifying.
## Copyright
Copyright (c) 2017 Libgirl Co,.Ltd.

## License
Licensed under the LLGPL License.
