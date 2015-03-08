;;;; cldm-registry.asd

(asdf:defsystem #:cldm-registry
  :description "CLDM registry application"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cldm
               #:cl-who
               #:hunchentoot
               #:sxql
               #:cl-dbi
	       #:parenscript)
  :serial t
  :components ((:file "package")
               (:file "cldm-registry")))

