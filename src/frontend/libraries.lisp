(in-package :cldm-registry.frontend)

(defun library-url (library)
  (format nil "libraries/~A" (model::name library)))

(restas:define-route libraries-handler ("/libraries")
  (with-frontend-common
    (:ul
     (loop for library in (model::list-all-libraries)
	  do
	  (who:htm
	   (:li (:a :href (library-url library) (who:str (model::name library)))
		(:p (model::description library))))))))

(restas:define-route  library-handler ("/libraries/:name")
  (let ((library (model::find-library-by-name name)))
    (if (not library)
	hunchentoot:+http-not-found+
	(with-frontend-common
	  (:p (who:str name))))))
