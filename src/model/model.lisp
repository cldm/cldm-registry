(in-package :cldm-registry.model)

(defparameter +db-host+ "localhost")
(defparameter +db-name+ "cldm-registry")
(defparameter +db-user+ "postgres")
(defparameter +db-pass+ "postgres")

(enable-sql-reader-syntax)

(defun connect-db ()
  (connect (list +db-host+ +db-name+ +db-user+ +db-pass+)
	   :database-type :postgresql :if-exists :new)
  (setf clsql:*DEFAULT-CACHING* nil))

(defun create-schema ()
  (create-view-from-class 'user)
  (create-sequence "user-id-seq"))

(defmethod store ((db-class t))
  (update-records-from-instance db-class))

(defun find-object (class id)
  (caar (clsql:select class :where [= [slot-value class 'id]
				id])))
