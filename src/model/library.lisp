(in-package :cldm-registry.model)

(def-view-class library ()
  ((id :accessor id
       :initarg :id
       :initform (sequence-next "library-id-seq")
       :type integer
       :db-kind :key
       :db-constraints (:not-null))
   (name :initarg :name
         :accessor name
         :initform nil
         :type string)
   (description :initarg :description
                :accessor description
                :initform nil
                :type string)
   (cld :initarg :cld
        :accessor cld
        :initform nil
        :type string)
   (logo :initarg :logo
         :accessor logo
         :initform nil
         :type string
         :documentation "Library logo pathname")
   (categories :initarg :categories
               :accessor categories
               :initform nil
               :type list)
   (keywords :initarg :keywords
             :accessor keywords
             :initform nil
             :type list)
   (authorid :initarg :authorid
             :accessor authorid
             :initform nil
             :type integer)
   (author :initarg :author
           :accessor author
           :initform nil
           :db-kind :join
           :db-info (:join-class user
                                 :home-key authorid
                                 :foreign-key id
                                 :set nil))
   (author-name :initarg :author-name
                :accessor author-name
                :initform nil
                :type string)
   (maintainerid :initarg :maintainerid
                 :accessor maintainerid
                 :initform nil
                 :type integer)
   (maintainer :initarg :maintainer
               :accessor maintainer
               :initform nil
               :db-kind :join
               :db-info (:join-class user
                                     :home-key maintainerid
                                     :foreign-key id
                                     :set nil))
   (maintainer-name :initarg :maintainer-name
                    :accessor maintainer-name
                    :initform nil
                    :type string)
   (licence :initarg :licence
            :accessor licence
            :initform nil
            :type string)
   (creation-time :initarg :creation-time
                  :accessor creation-time
                  :initform (get-time)
                  :type wall-time)))

(defmethod initialize-instance :after ((library library) &rest initargs)
  (when (stringp (cld library))
    (setf (cld library)
	  (cldm::parse-cld-address (cld library)))))

(def-view-class library-version ()
  ((id :accessor id
       :initarg :id
       :initform (sequence-next "library-version-id-seq")
       :type integer
       :db-kind :key
       :db-constraints (:not-null))
   (libraryid :initarg :libraryid
              :accessor libraryid
              :initform nil
              :type integer)
   (library :initarg :library
            :accessor library
            :initform nil
            :db-kind :join
            :db-info (:join-class library
                                  :home-key libraryid
                                  :foreign-key id
                                  :set nil))
   (version :initarg :version
            :accessor version
            :initform nil
            :type string)
   (description :initarg :description
                :accessor description
                :initform nil
                :type string)
   (stability :initarg :stability
              :accessor stability
              :initform nil
              :type string)
   (creation-time :initarg :creation-time
                  :accessor creation-time
                  :initform (get-time)
                  :type wall-time)
   (repositories :initarg :repositories
                 :initform (error "Provide a repository at least")
                 :accessor repositories
                 :documentation "Library version repositories")
   (dependencies :initarg :dependencies
                 :initform nil
                 :accessor dependencies
                 :documentation "The library version dependencies (list of requirement objects)")
   (provides :initarg :provides
             :initform nil
             :accessor provides
             :documentation "List of requirements the library provides"
             :type list)
   (conflicts :initarg :conflicts
              :initform nil
              :accessor conflicts
              :documentation "List of requirements the library is in conflict with"
              :type list)
   (replaces :initarg :replaces
             :initform nil
             :accessor replaces
             :documentation "List of requirements the library replaces"
             :type list)
   (suggests :initarg :suggests
             :initform nil
             :accessor suggests
             :documentation "List of requirements the library suggests"
             :type list)))

(defmethod initialize-instance :after ((library-version library-version) &rest initargs)
  (setf (version library-version) (cldm::read-version-from-string (version library-version)))
  (setf (repositories library-version)
	(mapcar #'cldm::unparse-repository (repositories library-version))))


(defun library-versions (library)
  #.(locally-enable-sql-reader-syntax)
  (sort 
   (select 'library-version :where 
	   [= [slot-value 'library-version 'library]
	   (id library)])
   #'semver:version<)
  #.(locally-disable-sql-reader-syntax))

(defun find-library-by-name (name)
  #.(locally-enable-sql-reader-syntax)
  (caar (select 'library :where
		[= [slot-value 'library 'name] name]))
  #.(locally-disable-sql-reader-syntax))

(defun publish (cldm-library)
  (let ((stored-library (find-library-by-name (cldm::library-name cldm-library))))
    (if stored-library
	(let ((library-version (first (cldm::library-versions cldm-library)))
	      (library-versions (library-versions stored-library)))
	  ;; If the library version to publish already exists, or is inferior
	  ;; to the currently published one, error
	  (if (or (member (cldm::version library-version) library-versions
			  :key #'semver:version
			  :test #'semver:version=)
		  (semver:version< (cldm::version library-version)
				   (first library-versions)))
	      (error "Invalid version ~A" library-version))
	  ;; Update the current library
	  (setf (description stored-library)
		(cldm::library-description cldm-library))
	  (setf (cld stored-library) (cldm::library-cld cldm-library))
	  (setf (licence stored-library) (cldm::library-licence cldm-library))
	  (setf (keywords stored-library) (cldm::library-keywords cldm-library)))
	;; else, create the library
	(let ((library (make-instance 'library 
				      :name (cldm::library-name cldm-library)
				      :cld (cldm::library-cld cldm-library)
				      :description (cldm::library-description cldm-library)
				      :keywords (cldm::library-keywords cldm-library)
				      :licence (cldm::library-licence cldm-library))))
	  (store library)
	  (let* ((cldm-library-version (first (cldm::library-versions cldm-library)))
		 (library-version (make-instance 'library-version
						 :library library
						 :libraryid (id library)
						 :description (cldm::description cldm-library-version)
						 :stability (cldm::stability cldm-library-version)
						 :version (cldm::version cldm-library-version)
						 :repositories (cldm::repositories cldm-library-version))))
	    (store library-version))))))
				       
				      
	
