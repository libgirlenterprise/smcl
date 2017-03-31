
(in-package :com.libgirl.smcl)

(defun smcl-run (procedure-literal-list output-file-pathname)
  (unless (atom procedure-literal-list) ; TODO: better way for exception handling
    (let* ((procedure-pool (make-instance 'procedure-pool :init-procedures procedure-literal-list))
	   (procedures (slot-value procedure-pool 'procedures)))
      (setf *user-input-function*
	    (make-user-input-function output-file-pathname procedure-pool))
      (loop for procedure-name being the hash-keys in procedures
	    do (let ((procedure (gethash procedure-name procedures)))
		 (setf (slot-value procedure 'body)
		       (or (reduce-f (slot-value procedure 'body)
				     procedure
				     procedure-pool)
			   (slot-value procedure 'body)))
		 (funcall *user-input-function*)))))) ; final chance to export to file


