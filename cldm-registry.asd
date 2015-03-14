;;;; cldm-registry.asd

(asdf:defsystem #:cldm-registry
  :description "CLDM registry application"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cldm
               #:cl-who
	       #:cl-mongo
	       #:parenscript
	       #:babel
	       #:ironclad
	       #:cl-smtp
	       #:uuid
	       #:anaphora
	       #:restas
	       #:restas-directory-publisher
	       ;#:djula
	       #:cl-forms.who
	       #:cl-secure-read
	       #:cl-json
	       #:session-token
	       #:montezuma)
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
				   (:file "app")
				   (:file "login")
				   (:file "register")
				   (:file "account")
				   (:file "libraries")
				   (:file "github")
				   (:file "api")))))))
