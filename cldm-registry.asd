;;;; cldm-registry.asd

(asdf:defsystem #:cldm-registry
  :description "CLDM registry application"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cldm
               #:cl-who
               #:hunchentoot
               #:clsql-postgresql
	       #:parenscript)
  :serial t
  :components ((:module :src
			:components
			((:file "package")
			 (:file "cldm-registry")
			 (:module :model
				  :components
				  ((:file "package")
				   (:file "user")))
			 (:module :frontend
				  :components
				  ((:file "package")
				   (:file "app")))))))
