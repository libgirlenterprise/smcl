
(in-package :com.libgirl.smcl)

(defun smcl-run (procedure-literal-list output-file-pathname)
  (unless (atom procedure-literal-list) ; TODO: better way for exception handling
    (let* ((procedure-pool (make-instance 'procedure-pool :init-procedures procedure-literal-list))
	   (procedures (slot-value procedure-pool 'procedures)))
      (setf (user-io-function procedure-pool)
	    (make-user-input-function output-file-pathname
				      procedure-pool))
      (loop for procedure-name being the hash-keys in procedures
	    do (reduce-f procedure-name
			 procedure-pool))
      (funcall (user-io-function procedure-pool))))) ; final chance to export to file


