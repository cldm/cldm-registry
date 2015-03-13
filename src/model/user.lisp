(in-package :cldm-registry.model)

(defclass user (mongo-model)
  ((username :initarg :username
	     :accessor username
	     :initform nil)
   (password :initarg :password
	     :accessor password
	     :initform nil)
   (email :initarg :email
	  :accessor email
	  :initform nil)
   (realname :initarg :realname
	     :accessor realname
	     :initform nil)
   (github-user :initarg :github-user
		:accessor github-user
		:initform nil)
   (github-token :initarg :github-token
		 :accessor github-token
		 :initform nil
		 :documentation "Github token. For user session. Not persisted")))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t :identity t)
    (format stream "~A" (username user))))

(defun load-user (doc)
  (make-instance 'user
		 :id (get-element :_id doc)
		 :uuid (get-element "uuid" doc)
		 :username (get-element "username" doc)
		 :password (get-element "password" doc)
		 :email (get-element "email" doc)
		 :realname (get-element "realname" doc)
		 :github-user (get-element "github-user" doc)))

(defun validate-user (user)
  (let ((errors nil))
    (flet ((verror (msg)
	     (push msg errors)))
      (if (null (username user))
	  (verror "Username required")
	  (when (not (stringp (username user)))
	    (verror "Invalid username")))
      (when (not (github-user user))
	(if (null (password user))
	    (verror "Password required")
	    (when (not (stringp (password user)))
	      (verror "Invalid password"))))
      (when (not (github-user user))
	(if (null (email user))
	    (verror "Email required")
	    (when (not (funcall (clavier:valid-email) (email user)))
	      (verror "Invalid email"))))
      (values (not errors) errors))))

(defmethod initialize-instance :after ((user user) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-bind (valid-p errors)
      (validate-user user)
    (when (not valid-p)
      (clavier:validation-error "User is invalid: ~A~%~A" user errors))))

(defun save-user (user)
  (when (not (doc user))
    (when (find-user-by-username (username user))
      (clavier:validation-error user "Username ~A is already taken" (username user))))
  (multiple-value-bind (valid-p errors)
      (validate-user user)
    (when (not valid-p)
      (clavier:validation-error user "Invalid user: ~A" errors)))
  (let ((doc (or (doc user)
		 (make-document))))
    (add-element "username" (username user) doc)
    (add-element "password" (password user) doc)
    (add-element "email" (email user) doc)
    (add-element "realname" (realname user) doc)
    (add-element "github-user" (github-user user) doc)

    (if (doc user)
	(db.save "users" doc)
	(progn
	  (add-element "uuid" (uuid user) doc)
	  (db.insert "users" doc)
	  (setf (doc user) doc)))))

(defun list-all-users ()
  (mapcar #'load-user (docs (db.find +users+ :all))))

(defun find-user (uuid)
  (awhen (first (docs (db.find +users+ (kv "uuid" uuid))))
    (load-user it)))

(defun find-user-by-username (username)
  (awhen (first (docs (db.find +users+ (kv "username" username))))
    (load-user it)))

(defun find-user-by-email (email)
  (awhen (first (docs (db.find +users+ (kv "email" email))))
    (load-user it)))

(defun user-login (username-or-email password)
  (awhen (first (docs (db.find +users+ ($and
					(kv "github-user" nil)
					($or (kv "username" username-or-email)
					     (kv "email" username-or-email))
					(kv "password" password)))))
    (load-user it)))

(defun github-user-login (username)
  (awhen (first (docs (db.find +users+ ($and
					(kv "github-user" t)
					(kv "username" username)))))
    (load-user it)))

(defmethod store ((user user))
  (save-user user))
