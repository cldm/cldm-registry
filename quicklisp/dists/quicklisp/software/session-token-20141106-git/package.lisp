;;;; package.lisp

(defpackage #:session-token
  (:use #:cl #:cl-isaac)
  (:export :make-generator :char-range
	   :init-common-lisp-random-seed
	   :init-kernel-seed))

