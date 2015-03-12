(in-package :cldm-registry.frontend)

(defparameter +host+ "localhost")
(defparameter +port+ 8090)

(defparameter *html* nil)

(defparameter *user* nil "The logged user")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-html (&body body)
    `(who:with-html-output (*html*)
       ,@body))

  (defmacro with-frontend-common (args &body body)
    `(call-with-frontend-common (lambda () (with-html ,@body)) ,@args)))

(defun start (&optional (host +host+) (port +port+))
  (model::connect-db)
  (restas:start '#:cldm-registry.frontend :hostname host
		:port port))

(restas:mount-module -static- (#:restas.directory-publisher)
  (:url "/static/")
  (restas.directory-publisher:*directory* 
   (asdf:system-relative-pathname :cldm-registry "src/frontend/static/"))
  (restas.directory-publisher:*autoindex* t))

(defun navbar (&key active)
  (with-html
    (:div :class "navbar navbar-default"
	  (:div :class "container"
		(:a :class "navbar-brand" :href "/" (who:str "CLDM registry"))
		(:ul :class "nav navbar-nav"
		     (:li :class (and (equalp active :home) "active")
			  (:a :href "/" (who:str "Home")))
		     (:li :class (and (equalp active :browse) "active")
			  (:a :href (restas:genurl 'libraries-handler)
			      (who:str "Browse")))
		     (if *user*
			 (who:htm (:li 
				   :class (and (equalp active :account) "active")
				   (:a :href (restas:genurl 'account-handler 
							    :name (model::username *user*))
				       (who:str (model::realname *user*)))))
			 (progn
			   (who:htm (:li :class (and (equalp active :login) "active")
					 (:a :href (restas:genurl 'login-handler)
					     (who:str "Sign up"))))
			   (who:htm (:li :class (and (equalp active :register) "active")
					 (:a :href (restas:genurl 'registration-handler)
					     (who:str "Register"))))))
		     (:li :class "divider-vertical")
		     (when *user*
		       (who:htm (:li (:a :href (restas:genurl 'logout)
					 (who:str "Logout"))))))))))

(defun call-with-frontend-common (function &rest args)
  (who:with-html-output-to-string (*html*)
    (:html
     (:head
      (:title "CLDM registry")
      (:meta :http-equiv "Content-Type"
	     :content "text/html; charset=UTF-8")
      (:link :rel "shortcut icon"
	     :href "/static/favicon.ico")
      (:link :rel "stylesheet"
	     :href "/static/style.css")
      (:link :rel "stylesheet" :href "/static/bower_components/bootstrap/dist/css/bootstrap.min.css")
      ;(:link :rel "stylesheet" :href "/static/bower_components/bootstrap/dist/css/bootstrap-theme.min.css")
      (:link :rel "stylesheet"
	     :href "/static/theme.css")
      (:script :type "javascript"
	       :src "/static/bower_components/jquery/dist/jquery.min.js")
      (:script :type "javascript" :src "/static/bower_components/bootstrap/dist/js/bootstrap.min.js"))
     (:body
      (apply #'navbar args)
      (:div :class "container"
	    (funcall function))))))

(restas:define-route main ("")
  (with-frontend-common (:active :home)
    (:div :class "jumbotron text-center"
	  (:h1 (who:str "Common Lisp Dependency Manager registry"))
	  (:p :class "lead" (who:str "CLDM is a distributed dependency manager for Common Lisp"))
	  (:p (:a :class "btn btn-large btn-success" 
		  :href (restas:genurl 'libraries-handler)
		  (who:str "Browse the libraries"))))
    (:div :class "row"
	  (:div :class "col-lg-6"
		(:h4 (who:str "Distributed"))
		(:p (who:str "Distributed by design"))
      
		(:h4 (who:str "Versions constraints"))
		(:p (who:str "Uses a Pseudo Boolean Expressions solver to manage version constraints")))
    
	  (:div :class "col-lg-6"
		(:h4 (who:str "VCS support"))
		(:p (who:str "Supports different Version Control Systems like Git, Darcs, Subversion, etc"))
      
		(:h4 (who:str "API"))
		(:p (who:str "Publish and manage your libraries via the API and the CLDM command line tool"))))))

(restas:define-route login-handler ("/login")
  (with-frontend-common (:active :login)
    (:a :href "/login/github" (who:str "Github login"))))

(restas:define-route registration-handler ("/registration")
  (with-frontend-common (:active :register)
    (:h1 "Register")
    (:form :action "/register" :method "POST"
	   (:div :class "form-group"
		 (:label :for "username" (who:str "Username"))
		 (:input :name "username" :class "form-control" :type "text" :placeholder "Enter username"))
	   (:div :class "form-group"
		 (:label :for "email" (who:str "Email"))
		 (:input :name "email" :class "form-control" :type "text" :placeholder "Enter an email"))
	   (:div :class "form-group"
		 (:label :for "password" (who:str "Password"))
		 (:input :name "password" :class "form-control" :type "password" :placeholader "Enter password"))
	   (:div :class "form-group"
		 (:label :for "realname" (who:str "Realname"))
		 (:input :name "realname" :class "form-control" :type "text" :placeholader "Enter realname"))
	   (:div :class "form-group"
		 (:input :type "submit" :value "Register")))))

(restas:define-route register-handler ("/register")
    (let ((username (hunchentoot:post-parameter "username"))
	  (email (hunchentoot:post-parameter "email"))
	  (password (hunchentoot:post-parameter "password"))
	  (realname (hunchentoot:post-parameter "realname")))
      (let* ((user-key (user-key :username username
				 :email email
				 :password password
				 :realname realname))
	     (validate-account-url (format nil "http://~A:~A/validate-account?k=~A"
					   +host+
					   +port+
					   user-key))
	     (message (format nil "You have requested a CLDM repository account.~%~%Visit this link to confirm: ~A" validate-account-url)))
	(cl-smtp:send-email "localhost" 
			    "noreply@cldm.org" 
			    email 
			    "Confirm CLDM acccount" 
			    message)
	(with-frontend-common () 
	  (:p (who:fmt "An email for account validation has been sent to ~A" email))))))

(restas:define-route validate-account-handler ("/validate-account")
  (let ((k (hunchentoot:get-parameter "k")))
    (let ((account (decode-string k)))
      (if (not account)
	  (with-frontend-common ()
	    (:p (who:str "Invalid registration key")))
	  ;; else
	  (progn
	    ;; Create the account here
	    (with-frontend-common ()
	      (:p (who:fmt "Your account has been created ~S" account))))))))
