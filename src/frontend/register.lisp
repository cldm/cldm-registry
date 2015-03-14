(in-package :cldm-registry.frontend)

(defparameter +smtp-host+ "localhost")
(defparameter +email-from+ "noreply@cldm.org")

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

(register-key "zgpaZoegGAbg3G480jn340fnsS3ia")

(defun user-key (&key username password email realname)
  (encode-string (format nil "(:username ~s :password ~s :email ~s :realname ~s)"
                         username
			 password
			 email
			 realname)))

(defun decode-token (token)
  (ignore-errors (read-from-string (decode-string token))))
