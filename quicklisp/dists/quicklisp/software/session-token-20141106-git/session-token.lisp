;;;; session-token.lisp
(in-package #:session-token)

(setf *random-state* (make-random-state t))

(defparameter *default-alphabet* (char-range (#\a :to #\z #\A :to #\Z #\0 :to #\9)))
(defparameter *default-token-length* 32)

(defun make-generator (&key (initial-seed (init-kernel-seed)) (alphabet *default-alphabet*) (token-length *default-token-length*))
  (let* ((len (length alphabet))
	 (next-pow (next-power-of-two len)))
    (flet ((next-char ()
	     (loop for b = (rand-bits initial-seed next-pow) until (> len b)
		finally (return b))))
      (lambda (&optional re-seed)
	(when re-seed (setf initial-seed re-seed))
	(let ((buf (make-string token-length)))
	  (loop for i from 0 repeat token-length
	     do (setf (aref buf i) (aref alphabet (next-char))))
	  buf)))))
