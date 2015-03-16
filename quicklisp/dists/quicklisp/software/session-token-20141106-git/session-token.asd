;;;; session-token.asd

(asdf:defsystem #:session-token
  :serial t
  :description "Simple session token generation library"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "BSD license: you can do anything you want with it (but no warranty)."
  :depends-on (#:cl-isaac)
  :components ((:file "package")
	       (:file "util")
               (:file "session-token")))

