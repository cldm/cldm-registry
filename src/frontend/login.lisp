(in-package :cldm-registry.frontend)

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
	  (:a :class "btn-auth btn-github large" 
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
