(in-package :cldm-registry.frontend)

(defparameter +host+ "localhost")
(defparameter +port+ 8090)

(defparameter *html* nil)

(defun start (&optional (host +host+) (port +port+))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				    :address host
				    :port port)))

(push (hunchentoot:create-folder-dispatcher-and-handler 
       "/static/"
       (asdf:system-relative-pathname :cldm-registry "src/frontend/static/"))
      hunchentoot:*dispatch-table*)

(defun call-with-frontend-common (function)
  (who:with-html-output-to-string (*html*)
    (:html
     (:head
      (:title "CLDM registry")
      (:meta :http-equiv "Content-Type"
	     :content "text/html; charset=UTF-8")
      (:link :rel "shortcut icon"
	     :href "static/favicon.ico")
      (:link :rel "stylesheet"
	     :href "static/style.css")
      (:link :rel "stylesheet" :href "static/bower_components/bootstrap/dist/css/bootstrap.min.css")
      (:link :rel "stylesheet" :href "static/bower_components/bootstrap/dist/css/bootstrap-theme.min.css")
      (:script :type "javascript"
	       :src "static/bower_components/jquery/dist/jquery.min.js")
      (:script :type "javascript" :src "static/bower_components/bootstrap/dist/js/bootstrap.min.js"))
     (:body
      (funcall function)))))

(defmacro with-html (&body body)
  `(who:with-html-output (*html*)
     ,@body))

(defmacro with-frontend-common (&body body)
  `(call-with-frontend-common (lambda () (with-html ,@body))))

(hunchentoot:define-easy-handler (index-handler :uri "/") ()
  (with-frontend-common
    (:h1 "Hello world")))

(hunchentoot:define-easy-handler (registration-handler :uri "/registration") ()
  (with-frontend-common
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

(hunchentoot:define-easy-handler (register-handler :uri "/register") ()
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
	(with-frontend-common 
	  (:p (who:fmt "An email for account validation has been sent to ~A" email))))))

(hunchentoot:define-easy-handler (validate-account-handler :uri "/validate-account")
    (k)
  (let ((account (decode-string k)))
    (if (not account)
	(with-frontend-common
	  (:p (who:str "Invalid registration key")))
	;; else
	(progn
	  ;; Create the account here
	  (with-frontend-common
	    (:p (who:fmt "Your account has been created ~S" account)))))))
