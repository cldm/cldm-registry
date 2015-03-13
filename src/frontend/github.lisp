(in-package :cldm-registry.frontend)

(defparameter +client-id+ "cee8bfdfe16aead7cd6a")
(defparameter +client-secret+ "04a3583b98caa012d417ad23c178f1dc9c9396b2")
(defparameter +state+ "a03na0823onf0n")
(defparameter +redirect-uri+ "http://localhost:8090/callback")

(restas:define-route github-login ("/login/github")
  (hunchentoot:redirect 
   (format nil "https://github.com/login/oauth/authorize?client_id=~A&redirect_uri=~A&scope=user&state=~A"
	   +client-id+
	   +redirect-uri+
	   +state+)))

(restas:define-route github-callback ("/callback")
  (let ((code (hunchentoot:get-parameter "code"))
	(state (hunchentoot:get-parameter "state")))
    (let ((stream
	   (drakma:http-request "https://github.com/login/oauth/access_token"
				:parameters `(("client_id" . ,+client-id+)
					      ("client_secret" . ,+client-secret+)
					      ("code" . ,code)
					      ("redirect_uri" . "http://localhost:8090/callback/done"))
				:accept "application/json"
				:method :post
				:want-stream t)))
      (let ((output (json:decode-json stream)))
	(let ((access-token (cdr (assoc :access--token output))))
	  (let ((stream (drakma:http-request "https://api.github.com/user"
					     :additional-headers (list (cons "Authorization" (format nil "token ~A" access-token)))
					     :want-stream t)))
	    (let ((user-data (json:decode-json stream)))
	      (let ((user (model::github-user-login 
			   (cdr (assoc :login user-data)))))
		(if user
		    ; the github user is registered, login
		    (progn
		      (setf (model::github-token user) access-token)
		      (setf (hunchentoot:session-value :user) user)
		      (restas:redirect 'main))
		    ; else, user not registered, register and login
		    (let ((user (make-instance 'model::user
					       :username (cdr (assoc :login user-data))
					       :email (cdr (assoc :email user-data))
					       :realname (cdr (assoc :name user-data))
					       :github-user t
					       :github-token access-token)))
		      (model::store user)
		      (setf (hunchentoot:session-value :user) user)
		      (restas:redirect 'main)))))))))))
