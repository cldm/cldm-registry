(in-package :cldm-registry.model)

(defun create-user (&key username
		      password
		      email
		      realname)
  (let ((user (make-document)))
    (add-element "uuid" (princ-to-string (uuid:make-v4-uuid)) user)
    (add-element "username" username user)
    (add-element "password" password user)
    (add-element "email" email user)
    (add-element "realname" realname user)
    (db.insert +users+ user)))

(defun list-all-users ()
  (docs (db.find +users+ :all)))

(defun find-user (uuid)
  (first (docs (db.find +users+ (kv "uuid" uuid)))))

(defun save-user (user)
  (db.save +users+ user))
