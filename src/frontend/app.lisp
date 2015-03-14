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

(defun stringp* (str)
  (and (stringp str)
       (not (zerop (length str)))
       str))

(defun start (&optional (host +host+) (port +port+))
  (model::connect-db)
  (restas:start '#:cldm-registry.frontend :hostname host
		:port port))

(restas:mount-module -static- (#:restas.directory-publisher)
  (:url "/static/")
  (restas.directory-publisher:*directory* 
   (asdf:system-relative-pathname :cldm-registry "src/frontend/static/"))
  (restas.directory-publisher:*autoindex* t))

;; Glyphicons fonts path hack. How to remove?
(restas:mount-module -fonts- (#:restas.directory-publisher)
  (:url "/fonts/")
  (restas.directory-publisher:*directory* 
   (asdf:system-relative-pathname :cldm-registry "src/frontend/static/bower_components/bootstrap/dist/fonts/"))
  (restas.directory-publisher:*autoindex* t))

(defclass logged-user-route (routes:proxy-route) ())

(defmethod restas:process-route :around ((route logged-user-route) bindings)
  (let ((*user* (hunchentoot:session-value :user)))
    (if *user*
	(call-next-method)
	(restas:redirect 'login-handler))))

(defun @logged-user (route)
  (make-instance 'logged-user-route :target route))

(defun navbar (&key active)
  (with-html
    (:div :class "navbar navbar-default"
	  (:div :class "container"
		(:a :class "navbar-brand" :href "/" (who:str "CLDM registry"))
		(:ul :class "nav navbar-nav"
		     (:li :class (and (equalp active :home) "active")
			  (:a :href (restas:genurl 'main) 
			      (:span :class "glyphicon glyphicon-home")
			      (who:str " Home")))
		     (:li :class (and (member active (list :browse :libraries))
				      "active")
			  (:a :href (restas:genurl 'libraries-handler)
			      (:span :class "glyphicon glyphicon-list-alt")
			      (who:str " Libraries")))
		     (:li :class (and (equalp active :users) "active")
			  (:a :href (restas:genurl 'users-handler)
			      (:span :class "glyphicon glyphicon-user")
			      (who:str " Users"))))
		(:ul :class "nav navbar-nav navbar-right"
		     (:li
		      (:form :class "navbar-form" :role "search"
			     (:div :class "input-group"
				   (:input :type "text" :class "form-control" 
					   :placeholder "Search" :name "term"
					   :id "srch-term"
					   (:div :class "input-group-btn"
						 (:button :class "btn btn-default" 
							  :type "submit"
							  (:i :class "glyphicon glyphicon-search")))))))
		     (if *user*
			 (who:htm (:li 
				   :class (and (equalp active :account) "active")
				   (:a :href (restas:genurl 'account-handler)
				       (:span :class "glyphicon glyphicon-cog")
				       (who:fmt " ~A" (or (stringp* (model::realname *user*))
							  (model::username *user*))))))
			 (progn
			   (who:htm (:li :class (and (equalp active :login) "active")
					 (:a :href (restas:genurl 'login-handler)
					     (:span :class "glyphicon glyphicon-log-in")
					     (who:str " Login"))))
			   (who:htm (:li :class (and (equalp active :register) "active")
					 (:a :href (restas:genurl 'registration-handler)
					     (:span :class "glyphicon glyphicon-pencil")
					     (who:str " Register"))))))
		     (:li :class "divider-vertical")
		     (when *user*
		       (who:htm (:li (:a :href (restas:genurl 'logout)
					 (:span :class "glyphicon glyphicon-log-out")
					 (who:str " Logout"))))))))))

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
		:href "/static/bower_components/css3-social-signin-buttons/auth-buttons.css")
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

(restas:define-route logout ("/logout")
  (setf (hunchentoot:session-value :user) nil)
  (restas:redirect 'main))
