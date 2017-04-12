
(in-package :com.libgirl.smcl)

(defmacro with-thread-check (form)
  `(cond
     ((null *smcl-thread*) (error "no smcl thread exist"))
     ((sb-thread:thread-alive-p *smcl-thread*) ,form)
     (t (error "no alive smcl threads"))))

(defun smcl-thread-run (procedure-literal-list)
  (setf *smcl-thread* ; make multiple procedure-pools in different thread in the future
	(sb-thread:make-thread (lambda ()
				 (if (null procedure-literal-list) ; TODO: better way for exception handling
				     (progn
				       (setf *hand-to-api-p* t)
				       (sb-thread:signal-semaphore *api-semaphore* 1)
				       (error "empty initial procedure-literal-list")
				       (sb-thread:return-from-thread :empty-list-p))
				     (let* ((procedure-pool (make-instance 'procedure-pool :init-procedures procedure-literal-list))
					    (procedures (slot-value procedure-pool 'procedures)))
				       (setf *smcl-error* nil)
				       (loop while t
					     do (loop for procedure-name being the hash-keys in procedures
						      do (reduce-f procedure-name
								   procedure-pool))))))))
  (sb-thread:thread-yield)
  (sb-thread:wait-on-semaphore *api-semaphore*)) ; WARNING: poor performance. WARNING: case no deeper reduce-f called

(defun smcl-run-steps (&key (step-count 1)) ; step-count should >= 1
  (with-thread-check (dotimes (i step-count t)
		       (setf *hand-to-api-p* nil)
		       (loop until *hand-to-api-p*
			     do (progn (sb-thread:signal-semaphore *api-semaphore* 1) ; WARNING: the performance will be very poor this way
				       (sb-thread:thread-yield)
				       (sb-thread:wait-on-semaphore *api-semaphore*))))))

(defun smcl-get-char ()
    *interface-char*)

(defun smcl-set-char (char-to-set)
  "Input a character into smcl and return the character. Return nil if the input given is not a character."
  (when (characterp char-to-set) 
    (setf *interface-char* char-to-set)))

(defun smcl-export (output-file-pathname)
  (with-thread-check (if (setf *export-pathname* output-file-pathname)
			 (loop while *export-pathname*
			       do (progn
				    (sb-thread:signal-semaphore *api-semaphore* 1) ; WARNING: the performance will be very poor this way
				    (sb-thread:thread-yield)
				    (sb-thread:wait-on-semaphore *api-semaphore*)))
			 (error "output file pathname null"))))

(defun smcl-thread-end ()
  (sb-thread:terminate-thread *smcl-thread*))

