(in-package :cldm-registry.frontend)

(defun print-user-name (user)
  (with-output-to-string (s)
    (format s "~A" (model::username user))
    (when (model::realname user)
      (format s " (~A)" (model::realname user)))))

(restas:define-route users-handler ("/users")
  (with-frontend-common (:active :users)
    (:h1 "Users")
    (:ul
     (loop for user in (model::list-all-users)
	do (who:htm (:li 
		     (:a :href (restas:genurl 'user-handler :username
					      (model::username user))
			 (who:str (print-user-name user)))))))))

(restas:define-route user-handler ("/users/:username")
  (let ((user (model::find-user-by-username username)))
    (when (not user)
      (return-from user-handler hunchentoot:+http-not-found+))
    (with-frontend-common (:active :users)
      (:h1 (who:str (print-user-name user)))
      (:h2 (who:str "Published libraries"))
      (:ul
       (loop for library in (model::published-libraries user)
	    do (who:htm (:li 
			 (:a :href (restas:genurl 'library-handler :name (model::name library))
			     (who:str (model::name library))))))))))
    
