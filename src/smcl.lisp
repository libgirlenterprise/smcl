
(in-package :com.libgirl.smcl)

(defun smcl-thread-run (procedure-literal-list output-file-pathname)
  (setf *smcl-thread*
	(sb-thread:make-thread (lambda ()
				 (unless (atom procedure-literal-list) ; TODO: better way for exception handling
				   (let* ((procedure-pool (make-instance 'procedure-pool :init-procedures procedure-literal-list))
					  (procedures (slot-value procedure-pool 'procedures)))
				     (loop while t
					   do (loop for procedure-name being the hash-keys in procedures
						    do (reduce-f procedure-name
								 procedure-pool)))))))))


