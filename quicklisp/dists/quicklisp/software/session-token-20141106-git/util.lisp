(in-package #:session-token)

(defmethod next-power-of-two ((num number))
  (loop for i from 1 for pow = (expt 2 i)
     when (>= pow num) return i))

(defmacro char-range ((&rest start-to-end-pairs) &key (plus) (not))
  (let ((res))
    (loop for (a to b) on start-to-end-pairs by #'cdddr
       do (let ((min (min (char-code a) (char-code b)))
		(max (max (char-code a) (char-code b))))
	    (loop for code from min to max for c = (code-char code)
	       unless (find c not) do (push c res))))
    (concatenate 'string (nreverse res) plus)))
