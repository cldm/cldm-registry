(in-package :cldm-registry.frontend)

(defparameter +host+ "localhost")
(defparameter +port+ 8090)

(defparameter *html* nil)

(defparameter +smtp-host+ "localhost")
(defparameter +email-from+ "noreply@cldm.org")

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
				   (:a :href (restas:genurl 'account-handler)
				       (who:str (model::realname *user*)))))
			 (progn
			   (who:htm (:li :class (and (equalp active :login) "active")
					 (:a :href (restas:genurl 'login-handler)
					     (who:str "Login"))))
			   (who:htm (:li :class (and (equalp active :register) "active")
					 (:a :href (restas:genurl 'registration-handler)
					     (who:str "Register"))))))
		     (:li :class "divider-vertical")
		     (when *user*
		       (who:htm (:li (:a :href (restas:genurl 'logout)
					 (who:str "Logout"))))))))))

(defun call-with-frontend-common (function &rest args)
  (let ((*user* (hunchentoot:session-value :user)))
    (who:with-html-output-to-string (*html*)
      (write-string "<!DOCTYPE html>" *html*)
      (who:htm
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
	 (:link :rel "stylesheet"
		:href "/static/bower_components/parsleyjs/src/parsley.css"))
	(:body
	 (:script :type "text/javascript"
		  :src "/static/bower_components/jquery/dist/jquery.min.js")
	 (:script :type "text/javascript" :src "/static/bower_components/bootstrap/dist/js/bootstrap.min.js")
	 (:script :type "text/javascript" :src "/static/bower_components/parsleyjs/dist/parsley.js")
	 (apply #'navbar args)
	 (:div :class "container"
	       (funcall function))
	 (:footer
	  (render-footer))))))))

(defun render-footer ()
  (with-html
    (:div :class "row"
	  (:div :class "col-md-2")
	  (:div :class "col-md-2"
		(:p (:a :href "https://github.com/mmontone" (who:str "© Mariano Montone"))
		    (who:str "  2015")))
	  (:div :class "col-md-2"
		(:ul (:li (:a :href "http://cldm.github.io/cldm" (who:str "CLDM")))
		     (:li (:a :href "https://github.com/cldm/cldm-registry" (who:str "CLDM Registry"))))))))

(defclass logged-user-route (routes:proxy-route) ())

(defmethod restas:process-route :around ((route logged-user-route) bindings)
  (let ((*user* (hunchentoot:session-value :user)))
    (if *user*
	(call-next-method)
	hunchentoot:+http-forbidden+)))

(defun @logged-user (route)
  (make-instance 'logged-user-route :target route))

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

(forms:defform account-form (:action (restas:genurl 'update-account)
				     :method :post)
  ((username :string :label "Username" :required-p t)
   (email :email :label "Email" :required-p t)
   (password :password :label "Password" :required-p t)
   (confirm-password :password :label "Confirm password" :required-p nil)
   (realname :string :label "Realname" :required-p nil)
   (submit :submit :label "Update")))

(defun render-account-page (form)
  (with-frontend-common ()
    (:h1 (who:str "Your account"))
    (forms:with-form-renderer :who
      (let ((forms.who::*html* *html*))
	(forms:with-form form
	  (forms:render-form-errors)
	  (forms:render-form-start)
	  (with-html
	    (:div :class "form-group"
		  (forms:render-field-label 'username form :for "username")
		  (forms:render-field-widget 'username form 
					     :class "form-control col-lg-1"
					     :placeholder "Enter username"))
	    (:div :class "form-group"
		  (forms::render-field-label 'email form :for "email")
		  (forms:render-field-widget 'email form 
					     :class "form-control col-lg-1"
					     :placeholder "Enter an email"))
	    (:div :class "form-group"
		  (forms::render-field-label 'password form :for "password")
		  (forms:render-field-widget 'password form
					     :class "form-control col-lg-1" 
					     :placeholader "Enter password"))
	    (:div :class "form-group"
		  (forms::render-field-label 'confirm-password form
					     :for "confirm password")
		  (forms:render-field-widget 'confirm-password form
					     :class "form-control col-lg-1" 
					     :placeholader "Confirm password"))

	    (:div :class "form-group"
		  (forms::render-field-label 'realname form :for "realname")
		  (forms:render-field-widget 'realname form 
					     :class "form-control col-lg-1"
					     :placeholder "Enter realname"))
	    (:div :class "form-group"
		  (forms:render-field-widget 'submit)))
	  (forms:render-form-end))))
    (:h1 "API token")
    (:p (:pre (:code (who:str (model::api-token *user*)))))))

(restas:define-route account-handler ("/account")
  (:decorators '@logged-user)
  (let ((form (forms:get-form 'account-form)))
    (forms:with-form-fields (username password email realname)
	form
	(setf (forms:field-value username) (model::username *user*))
	(setf (forms:field-value password) (model::password *user*))
	(setf (forms:field-value email) (model::email *user*))
	(setf (forms:field-value realname) (model::realname *user*)))
    (render-account-page form)))

(restas:define-route update-account ("/account" :method :post)
  (:decorators '@logged-user)
  (let ((form (forms:get-form 'account-form)))
    (forms:with-form form
      (forms:handle-request)
      (if (not (forms:validate-form))
	  (render-account-page form)
					; else
	  (forms:with-form-field-values (username password confirm-password email realname)
	      form
	    (cond
	      ((and (string-not-equal username (model::username *user*))
		    (model::find-user-by-username username))
	       (push (cons (forms::get-field form 'username)
			   (list "Username already taken. Choose another."))
		     (forms::form-errors form))
	       (render-account-page form))
	      ((and (string-not-equal (model::password *user*)
				      password)
		    (not (string= password confirm-password)))
	       (push (cons (forms::get-field form 'confirm-password)
			   (list "Passwords don't match"))
		     (forms::form-errors form))
	       (render-account-page form))
	      ;; else, valid form, update the user account
	      (t
	       (setf (model::username *user*) username)
	       (setf (model::password *user*) password)
	       (setf (model::email *user*) email)
	       (setf (model::realname *user*) realname)
	       (model::store *user*)
	       (restas:redirect 'account-handler))))))))

(restas:define-route logout ("/logout")
  (setf (hunchentoot:session-value :user) nil)
  (restas:redirect 'main))

(forms:defform login-form (:action (restas:genurl 'login-action)
					  :method :post)
  ((username-or-email :string :label "Username or email" :required-p t)
   (password :password :label "Password" :required-p t)
   (submit :submit :label "Login")))

(defun render-login-form (form &optional errors)
  (with-frontend-common (:active :login)
    (:div :class "row" :style  "margin: 0 auto; width:50%"
	  (:h1 "Login")
	  (forms:with-form-renderer :who
	    (let ((forms.who::*html* *html*))
	      (forms:with-form form
		(when errors
		  (with-html
		    (:ul
		     (loop for error in errors
			do (who:htm (:li (who:str error)))))))
		(forms:render-form-errors)
		(forms:render-form-start)
		(with-html
		  (:div :class "form-group"
			(forms:render-field-label 'username-or-email form :for "username-or-email")
			(forms:render-field-widget 'username-or-email form 
						   :class "form-control col-lg-1"
						   :placeholder "Enter username or email"))
		  (:div :class "form-group"
			(forms::render-field-label 'password form :for "password")
			(forms:render-field-widget 'password form
						   :class "form-control col-lg-1" 
						   :placeholader "Enter password"))
		  (:div :class "form-group"
			(forms:render-field-widget 'submit)))
		(forms:render-form-end))))
	  (:a :class "btn btn-large btn-success" 
	      :href (restas:genurl 'github-login) 
	      (who:str "Login with Github")))))

(restas:define-route login-handler ("/login")
  (let ((form (forms:get-form 'login-form)))
    (render-login-form form)))

(restas:define-route login-action ("/login" :method :post)
  (let ((form (forms:get-form 'login-form)))
    (forms:with-form form
      (forms:handle-request)
      (if (not (forms:validate-form))
	  ;; invalid form
	  (render-login-form form)
	  ;; else
	  (forms:with-form-field-values (username-or-email password) 
	      form
	    (let ((user (model::user-login username-or-email password)))
	    (if (not user)
		;; invalid login
		(render-login-form form (list "Invalid username or password"))
		;; else, succesful login
		(progn
		  (setf (hunchentoot:session-value :user) user)
		  (restas:redirect 'main)))))))))

(forms:defform registration-form (:action (restas:genurl 'register-handler)
					  :method :post)
  ((username :string :label "Username" :required-p t)
   (email :email :label "Email" :required-p t)
   (password :password :label "Password" :required-p t)
   (confirm-password :password :label "Confirm password" :required-p t)
   (realname :string :label "Realname" :required-p nil)
   (submit :submit :label "Register")))

(defun render-registration-page (form)
  (with-frontend-common (:active :register)
    (:div :class "row" :style  "margin: 0 auto; width:50%"
	  (:h1 "Register")
	  (forms:with-form-renderer :who
	    (let ((forms.who::*html* *html*))
	      (forms:with-form form
		(forms:render-form-errors)
		(forms:render-form-start)
		(with-html
		  (:div :class "form-group"
			(forms:render-field-label 'username form :for "username")
			(forms:render-field-widget 'username form 
						   :class "form-control col-lg-1"
						   :placeholder "Enter username"))
		  (:div :class "form-group"
			(forms::render-field-label 'email form :for "email")
			(forms:render-field-widget 'email form 
						   :class "form-control col-lg-1"
						   :placeholder "Enter an email"))
		  (:div :class "form-group"
			(forms::render-field-label 'password form :for "password")
			(forms:render-field-widget 'password form
						   :class "form-control col-lg-1" 
						   :placeholader "Enter password"))
		  (:div :class "form-group"
			(forms::render-field-label 'confirm-password form
						   :for "confirm password")
			(forms:render-field-widget 'confirm-password form
						   :class "form-control col-lg-1" 
						   :placeholader "Confirm password"))

		  (:div :class "form-group"
			(forms::render-field-label 'realname form :for "realname")
			(forms:render-field-widget 'realname form 
						   :class "form-control col-lg-1"
						   :placeholder "Enter realname"))
		  (:div :class "form-group"
			(forms:render-field-widget 'submit)))
		(forms:render-form-end)))))))

(restas:define-route registration-handler ("/register")
  (let ((form (forms:get-form 'registration-form)))
    (render-registration-page form)))

(restas:define-route register-handler ("/register" :method :post)
  (let ((form (forms:get-form 'registration-form)))
    (forms:with-form form
      (forms:handle-request)
      (if (not (forms:validate-form))
	  (render-registration-page form)
	  ;; else
	  (forms:with-form-field-values (username email password 
						  confirm-password realname) 
	      form
	    (cond 
	      ((model::find-user-by-username username)
	       (push (cons (forms::get-field form 'username)
			   (list "Username already taken. Choose another."))
		     (forms::form-errors form))
	       (render-registration-page form))
	      ((not (string= password confirm-password))
	       (push (cons (forms::get-field form 'confirm-password)
			   (list "Passwords don't match"))
		     (forms::form-errors form))
	       (render-registration-page form))
	      ;; else, valid form
	      (t
	       (let* ((user-key (user-key :username username
					  :email email
					  :password password
					  :realname realname))
		      (validate-account-url (format nil "http://~A:~A/validate-account?k=~A"
						    +host+
						    +port+
						    user-key))
		      (message (format nil "You have requested a CLDM repository account.~%~%Visit this link to confirm: ~A" validate-account-url)))
		 (cl-smtp:send-email +smtp-host+
				     +email-from+
				     email 
				     "Confirm CLDM registry account" 
				     message)
		 (with-frontend-common () 
		   (:p (who:fmt "An email for account validation has been sent to ~A" email)))))))))))

(restas:define-route validate-account-handler ("/validate-account")
  (let ((k (hunchentoot:get-parameter "k")))
    (let ((account (read-from-string (decode-string k))))
      (if (not account)
	  (with-frontend-common ()
	    (:p (who:str "Invalid registration key")))
	  ;; else
	  (progn
	    ;; Create the user account
	    (let ((user (make-instance 'model::user
				       :username (getf account :username)
				       :password (getf account :password)
				       :email (getf account :email)
				       :realname (getf account :realname))))
	      (model::store user)
	      (setf (hunchentoot:session-value :user) user)
	      (restas:redirect 'main)))))))
