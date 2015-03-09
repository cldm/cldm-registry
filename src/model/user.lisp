(in-package :cldm-registry.model)

(def-view-class user ()
  ((id :accessor id 
       :initarg :id
       :initform (sequence-next "user-id-seq")
       :type integer 
       :db-kind :key 
       :db-constraints (:not-null))
   (username :initarg :username
	     :accessor username
	     :initform nil
	     :type string)
   (password :initarg :password
	     :accessor password
	     :initform nil
	     :type string)
   (realname :initarg :realname
	     :accessor realname
	     :initform nil
	     :type string)
   (email :initarg :email
	  :accessor email
	  :initform nil
	  :type string)
   (github-token :initarg :github-token
		 :accessor github-token
		 :initform nil
		 :type string)
   (registration-time :initarg :registration-time
		      :accessor registration-time
		      :initform (get-time)))
  (:base-table "registry-user"))
