
(in-package :com.libgirl.smcl)

(defun smcl-thread-run (procedure-literal-list)
  (setf *smcl-thread* ; make multiple procedure-pools in different thread in the future
	(sb-thread:make-thread (lambda ()
				 (unless (atom procedure-literal-list) ; TODO: better way for exception handling
				   (let* ((procedure-pool (make-instance 'procedure-pool :init-procedures procedure-literal-list))
					  (procedures (slot-value procedure-pool 'procedures)))
				     (loop while t
					   do (loop for procedure-name being the hash-keys in procedures
						    do (reduce-f procedure-name
								 procedure-pool))))))))
  (sb-thread:wait-on-semaphore *api-semaphore*)) ; WARNING: poor performance

(defun smcl-run-steps (&key (step-count 1)) ; step-count should >= 1
  (dotimes (i step-count t)
    (setf *lock-reduction-p* nil)
    (sb-thread:signal-semaphore *api-semaphore*) ; WARNING: the performance will be very poor this way
    (sb-thread:wait-on-semaphore *api-semaphore*)))

(defun smcl-get-char ()
    *interface-char*)

(defun smcl-set-char (char-to-set)
  "Input a character into smcl and return the character. Return nil if the input given is not a character."
  (when (characterp char-to-set) 
    (setf *interface-char* char-to-set)))

(defun smcl-export (output-file-pathname)
  (setf *export-pathname* output-file-pathname)
  (sb-thread:signal-semaphore *api-semaphore*) ; WARNING: the performance will be very poor this way
  (sb-thread:wait-on-semaphore *api-semaphore*)
  (setf *export-pathname* nil))

