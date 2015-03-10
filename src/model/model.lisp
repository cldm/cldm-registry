(in-package :cldm-registry.model)

(defparameter +db-host+ "localhost")
(defparameter +db-name+ "cldm-registry")
(defparameter +db-user+ "postgres")
(defparameter +db-pass+ "postgres")

(defun connect-db ()
  (connect (list +db-host+ +db-name+ +db-user+ +db-pass+)
	   :database-type :postgresql :if-exists :new)
  (setf *DEFAULT-CACHING* nil))

(defun create-schema ()
  (create-view-from-class 'user)
  (create-sequence "user-id-seq")
  (create-view-from-class 'category)
  (create-sequence "category-id-seq")
  (create-view-from-class 'library)
  (create-sequence "library-id-seq")
  (create-view-from-class 'library-version)
  (create-sequence "library-version-id-seq"))

(defmethod store ((db-class t))
  (update-records-from-instance db-class))

(defun find-object (class id)
  #.(locally-enable-sql-reader-syntax)
  (caar (select class :where [= [slot-value class 'id]
		id]))
  #.(locally-disable-sql-reader-syntax))

(defun parse-library (librarydef)
  (destructuring-bind (deflibrary name &body options) librarydef
    (assert (or (equalp deflibrary 'cldm:deflibrary)
		(equalp deflibrary 'cl-user::deflibrary)))
    (destructuring-bind (&key author maintainer description
			      licence cld version keywords &allow-other-keys)
	options
      (if version
	  (progn
	    (check-type author (or null string))
	    (check-type maintainer (or string null))
	    (check-type description (or null string))
	    (check-type licence (or null string))
	    (check-type keywords (or cons null))
	    ;; In this case, we assume the latest version of the library is being specified
	    (let ((library
		   (make-instance 'cldm::library
				  :name (if (symbolp name)
					    (string-downcase (symbol-name name))
					    name)
				  :author author
				  :maintainer maintainer
				  :description description
				  :licence licence
				  :cld (cldm::parse-cld-address cld)
				  :keywords keywords
				  :versions nil))
		  (library-version (make-instance 'cldm::library-version
						  :version (cldm::read-version-from-string (getf options :version))
						  :stability (getf options :stability)
						  :repositories (cldm::parse-version-repositories (getf options :repositories))
						  :dependencies (cldm::parse-version-dependencies (getf options :depends-on)))))
	      (setf (cldm::library library-version) library)
	      (setf (cldm::library-versions library)
		    (list library-version))
	      library))
	  ;; else, we assume it is the full version
	  (cldm::parse-library librarydef)))))  

(defun read-library-from-string (string)
  (parse-library (cldm::secure-read-from-string string)))
