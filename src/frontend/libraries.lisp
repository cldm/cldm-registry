(in-package :cldm-registry.frontend)

(defun library-url (library)
  (format nil "libraries/~A" (model::name library)))

(restas:define-route libraries-handler ("/libraries")
  (with-frontend-common (:active :browse)
    (:ul
     (loop for library in (model::list-all-libraries)
	  do
	  (who:htm
	   (:li (:a :href (library-url library) (who:str (model::name library)))
		(:p (model::description library))))))))

(defun print-library-version-to-string (library-version)
  (format nil "~A-~A" (model::name (model::library library-version))
	  (semver:print-version-to-string
	   (model::version library-version))))

(restas:define-route  library-handler ("/libraries/:name")
  (let ((library (model::find-library-by-name name)))
    (if (not library)
	hunchentoot:+http-not-found+
	(with-frontend-common (:active :browse)
	  (:h1 (who:str name))
	  (:p (who:str (model::description library)))
	  (:p (who:fmt "Author: ~A" (model::author library)))
	  (:p (who:fmt "Licence: ~A" (model::licence library)))
	  (:p (who:str "CLD: ")
	      (:a :href (restas:genurl 'library-cld-handler :name name)
		  (who:fmt "~A.cld" name)))
	  (:ul (loop for library-version in (model::library-versions library)
		    do (who:htm
			(:li (:a :href (restas:genurl 'library-version-handler 
						      :name name 
						      :version (semver:print-version-to-string (model::version library-version)))
				 (who:str (print-library-version-to-string library-version)))))))))))

(restas:define-route library-version-handler ("/versions/:name/:version")
  (let ((library-version (model::find-library-version name version)))
    (if (not library-version)
	hunchentoot:+http-not-found+
	(with-frontend-common (:active :browse)
	  (:h1 (who:str (print-library-version-to-string library-version)))
	  (:p (who:str (model::description library-version)))))))

(restas:define-route library-cld-handler ("/libraries/:(name).cld")
  (let ((library (model::find-library-by-name name)))
    (if (not library)
	hunchentoot:+http-not-found+
	(progn
	  (setf (hunchentoot:header-out "Content-Type") "text")
	  (model::print-library-definition library)))))
