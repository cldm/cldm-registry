(in-package :cldm-registry.model)

(defparameter +mongo+ "cldm-registry")
(defparameter +users+ "users")
(defparameter +libraries+ "libraries")
(defparameter +versions+ "versions")

(defclass mongo-model ()
  ((id :accessor id
       :initarg :id
       :initform nil)
   (doc :initarg :doc
	:accessor doc
	:initform nil)	
   (uuid :accessor uuid
	 :initarg :uuid
	 :initform (princ-to-string (uuid:make-v4-uuid)))))

(defmethod make-bson-oid ((oid vector))
  (mongo::make-bson-oid :oid oid))

(defmethod make-bson-oid ((oid mongo::bson-oid))
  oid)

(defun connect-db ()
  (db.use +mongo+))
