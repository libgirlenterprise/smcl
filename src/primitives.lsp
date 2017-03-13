
(let ((primitives nil))
  (setf primitives (make-hash-table))
  (setf (gethash 'quote primitives)
       (lambda (a) (* a 100)))
  (funcall (gethash 'quote  primitives) 33 22))
  
