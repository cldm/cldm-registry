(in-package :cldm-registry.frontend)

(restas:define-route libraries-handler ("/libraries")
  (with-frontend-common (:active :browse)
    (:h1 (who:str "Libraries"))
    (:ul
     (loop for library in (model::list-all-libraries)
        do
          (who:htm
           (:li (:a :href (restas:genurl 'library-handler :name (model::name library))
		    (who:str (model::name library)))
                (who:fmt " - ~A" (model::description library))))))))

(defun print-library-version-to-string (library-version)
  (format nil "~A-~A" (model::name (model::library library-version))
          (semver:print-version-to-string
           (model::version library-version))))

(restas:define-route library-handler ("/libraries/:name")
  (let ((library (model::find-library-by-name name)))
    (if (not library)
        hunchentoot:+http-not-found+
        (with-frontend-common (:active :browse)
          (:div :class "row"
                (:h1 (who:str name))
                (:p (who:str (model::description library)))
                (:p (:b (who:str "Author: ")) (who:fmt "~A" (model::author library)))
		(:p (:b (who:str "Categories: "))
		    (let ((categories (model::categories library)))
		      (loop for category in (butlast categories)
			 do
			   (who:htm (:a :href (restas:genurl 'category-handler :name category)
					(who:str category))
				    (who:str ", ")))
		      (let ((category (car (last categories))))
			(who:htm (:a :href (restas:genurl 'category-handler :name category)
				     (who:str category))))))
		(:p (:b (who:str "Keywords: "))
		    (let ((keywords (model::keywords library)))
		      (loop for keyword in (butlast keywords)
			 do
			   (who:htm (:a :href (restas:genurl 'keyword-handler :name keyword)
					(who:str keyword))
				    (who:str ", ")))
		      (let ((keyword (car (last keywords))))
			(who:htm (:a :href (restas:genurl 'keyword-handler :name keyword)
				     (who:str keyword))))))
                (:p (:b (who:str "Licence: ")) (who:fmt "~A" (model::licence library)))
		(let ((latest-version (first (model::library-versions library))))
		  (who:htm
		   (:p (:b (who:str "Latest version: "))
		       (:a :href (restas:genurl 'library-version-handler 
						:name (model::name library)
						:version (semver:print-version-to-string 
							  (model::version latest-version)))
			   (who:str (semver:print-version-to-string 
				     (model::version latest-version)))))
					       
		   (:p (:b (who:str "Dependencies: "))
		       (let ((deps (model::dependencies latest-version)))
			 (loop for dep in (butlast deps)
			    do
			      (who:htm (render-requirement dep)
				       (who:str ", ")))
			 (let ((dep (car (last deps))))
			   (render-requirement dep))))))
		(let ((publisher (model::publisher library)))
		  (who:htm
		   (:p (:b (who:str "Publisher: "))
		       (:a :href (restas:genurl 'user-handler 
						:username 
						(model::username publisher))
			   (who:str (print-user-name publisher))))))
		(:p (:b (who:str "Published: "))
		    (who:str (mongo::fmt (model::creation-time library) nil)))
		(:p (:b (who:str "Last update: "))
		    (who:str (mongo::fmt (model::update-time library) nil)))
                (:p (:b (who:str "CLD: "))
                    (:a :href (restas:genurl 'library-cld-handler :name name)
                        (who:fmt "~A.cld" name))))
          (:div :class "row"
                (render-library-versions library))))))

(defun render-library-versions (library)
  (with-html
    (:h3 (who:str "Versions:"))
    (:ul :id "versions"
	 (loop for library-version in (model::library-versions library)
	    do (who:htm 
		(:li (:a :href (restas:genurl 'library-version-handler
					      :name (model::name (model::library library-version))
					      :version (semver:print-version-to-string
							(model::version library-version)))
			 (who:str (print-library-version-to-string library-version)))
		     (who:fmt " - ~A" (mongo::fmt (model::creation-time library-version) nil))
		     ))))))

(defun render-library-version (library-version)
  (let ((version (semver:print-version-to-string
                  (model::version library-version))))
    (with-html
      (:div :class "panel panel-default"
            (:div :class "panel-heading" :role "tab"
                  :id (format nil "heading-~A" version)
                  (:h4 :class"panel-title"
                       (:a :data-toggle "collapse"
                           :data-parent="#versions"
                           :href (format nil "#collapse-~A" version)
                           :aria-expanded "false"
                           :aria-controls (format nil "collapse-~A" version)
                           (who:str version))))
            (:div :id  (format nil "collapse-~A" version)
                  :class "panel-collapse collapse in"
                  :role "tabpanel"
                  :aria-labelledby (format nil "heading-~A" version)
                  (:div :class "panel-body"
                        (:a :href (restas:genurl 'library-version-handler
						 :name (model::name (model::library library-version))
						 :version version)
			    (who:str (print-library-version-to-string library-version)))))))))

(defun render-requirement (requirement)
  (flet ((render-version-constraint (version-constraint)
	   (when (not (equalp version-constraint :any))
	     (destructuring-bind (operation version) version-constraint
	       (with-html
		 (who:fmt " ~A ~A"
			  operation
			  (semver:print-version-to-string version)))))))
    (with-html
      (:a :href (restas:genurl 'library-handler :name  (cldm::library-name requirement))
	  (who:str (cldm::library-name requirement)))
      (let ((version-constraint (first (cldm::requirement-version-constraints requirement))))
	(when version-constraint
	  (render-version-constraint version-constraint)))

      (loop for version-constraint in (rest (cldm::requirement-version-constraints requirement))
	 do (who:htm
	     (who:str ", ")
	     (:a :href (restas:genurl 'library-handler :name  (cldm::library-name requirement))
		 (who:str (cldm::library-name requirement)))
	     (render-version-constraint version-constraint))))))

(defgeneric render-repository-address (repository-address)
  (:method ((repository-address cldm::directory-repository-address))
    (with-html
      (who:str (cldm::directory repository-address))))
  (:method ((repository-address cldm::url-repository-address))
    (with-html
      (:a (:href (cldm::url repository-address)))))      
  (:method ((repository-address cldm::git-repository-address))
    (with-html
      (who:str "git ")
      (:a :href (cldm::url repository-address)
	  (who:str (cldm::url repository-address)))
      (awhen (cldm::commit repository-address)
	(who:fmt " commit: ~A" it))
      (awhen (cldm::branch repository-address)
	(who:fmt " branch: ~A" it))
      (awhen (cldm::tag repository-address)
	(who:fmt " tag: ~A" it))))
  (:method ((repository-address cldm::ssh-repository-address))
    (with-html
      (who:str "ssh ")
      (:a :href (cldm::address repository-address)
	  (who:str (cldm::address repository-address)))))
  (:method ((repository-address cldm::darcs-repository-address))
    (with-html 
      (who:str "darcs ")
      (:a :href (cldm::url repository-address)
	  (who:str (cldm::url repository-address))))))

(restas:define-route library-version-handler ("/versions/:name/:version")
  (let ((library-version (model::find-library-version name version)))
    (if (not library-version)
        hunchentoot:+http-not-found+
	(let ((library (model::library library-version)))
	  (with-frontend-common (:active :browse)
	    (:div :class "row"
		  (:h1 (who:str (print-library-version-to-string library-version)))
		  (:p (who:str (model::description library-version)))
		  (:p (who:str (model::description library)))
		  (:p (:b (who:str "Library: "))
		      (:a :href (restas:genurl 'library-handler
					       :name (model::name library))
			  (who:str (model::name library))))
		  (:p (:b (who:str "Author: ")) (who:fmt "~A" (model::author library)))
		  (:p (:b (who:str "Categories: "))
		      (let ((categories (model::categories library)))
			(loop for category in (butlast categories)
			   do
			     (who:htm (:a :href (restas:genurl 'category-handler :name category)
					  (who:str category))
				      (who:str ", ")))
			(let ((category (car (last categories))))
			  (who:htm (:a :href (restas:genurl 'category-handler :name category)
				       (who:str category))))))
		  (:p (:b (who:str "Keywords: "))
		      (let ((keywords (model::keywords library)))
			(loop for keyword in (butlast keywords)
			   do
			     (who:htm (:a :href (restas:genurl 'keyword-handler :name keyword)
					  (who:str keyword))
				      (who:str ", ")))
			(let ((keyword (car (last keywords))))
			  (who:htm (:a :href (restas:genurl 'keyword-handler :name keyword)
				       (who:str keyword))))))
		  (:p (:b (who:str "Licence: ")) (who:fmt "~A" (model::licence library)))
		  (:p (:b (who:str "Version: "))
		      (who:str (semver:print-version-to-string 
				(model::version library-version))))
		  (:p (:b (who:str "Dependencies: "))
		      (let ((deps (model::dependencies library-version)))
			(loop for dep in (butlast deps)
			   do
			     (who:htm (render-requirement dep)
				      (who:str ", ")))
			(let ((dep (car (last deps))))
			  (render-requirement dep))))
		  (let ((publisher (model::publisher library)))
		    (who:htm
		     (:p (:b (who:str "Publisher: "))
			 (:a :href (restas:genurl 'user-handler 
						  :username 
						  (model::username publisher))
			     (who:str (print-user-name publisher))))))
		  (:p (:b (who:str "Published: "))
		      (who:str (mongo::fmt (model::creation-time library-version) nil)))
		  (:p (:b (who:str "Repositories: "))
		      (:ul (loop for repo in (model::repositories library-version)
				do (who:htm (:li (:b (who:fmt "~A: " (cldm::name repo)))
						 (render-repository-address (cldm::repository-address repo)))))))))))))

(restas:define-route library-cld-handler ("/libraries/:(name).cld")
  (let ((library (model::find-library-by-name name)))
    (if (not library)
        hunchentoot:+http-not-found+
        (progn
          (setf (hunchentoot:header-out "Content-Type") "text")
          (model::print-library-definition library)))))

(restas:define-route search-handler ("/search" :method :post)
  (let ((q (hunchentoot:post-parameter "q")))
    (let ((results (model::search-library (format nil "name:\"~A\"" q))))
      (with-frontend-common ()
	(if (not results)
	    (who:htm (:h1 "No results"))
	    (who:htm
	     (:h1 "Search results")
	     (:ul
	      (loop for result in results do
		   (let ((library (cdr (assoc :library result))))
		     (who:htm (:li (:a :href (restas:genurl 'library-handler :name (model::name library))
				       (who:str (model::name library)))
				   (who:fmt " - ~A" (model::description library)))))))))))))
