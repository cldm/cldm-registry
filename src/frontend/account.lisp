(in-package :cldm-registry.frontend)

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
    (:div :class "row" :style  "margin: 0 auto; width:50%"
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
	  (:p (:pre (:code (who:str (model::api-token *user*)))))
	  (:h1 "Your libraries")
	  (let ((libraries (model::published-libraries *user*)))
	    (if (not libraries)
		(who:htm (:h3 (who:str "You have no libraries")))
		(who:htm 
		 (:ul
		  (loop for library in libraries
		     do
		       (who:htm (:li (:a :href (restas:genurl 'library-handler :name (model::name library))
					 (who:str (model::name library)))))))))))))


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
