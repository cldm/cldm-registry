;;;; cldm-registry.asd

(asdf:defsystem #:cldm-registry
  :description "CLDM registry application"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cldm
               #:cl-who
               #:hunchentoot
               #:clsql-postgresql
	       #:parenscript
	       #:babel
	       #:ironclad
	       #:cl-smtp)
  :serial t
  :components ((:module :src
			:components
			((:file "package")
			 (:file "cldm-registry")
			 (:module :model
				  :components
				  ((:file "package")
				   (:file "model")
				   (:file "user")
				   (:file "category")
				   (:file "library")))
			 (:module :frontend
				  :components
				  ((:file "package")
				   (:file "encrypt")
				   (:file "app")))))))
