(in-package :cldm-registry.model)

(defclass mongo-model ()
  ((id :accessor id
       :initarg :id
       :initform nil
       :type integer)
   (doc :initarg :doc
	:accessor doc
	:initform nil)	
   (uuid :accessor uuid
	 :initarg :uuid
	 :initform (princ-to-string (uuid:make-v4-uuid)))))
       
(defclass library (mongo-model)
  ((name :initarg :name
         :accessor name
         :initform nil
         :type string)
   (description :initarg :description
                :accessor description
                :initform nil
                :type string)
   (cld :initarg :cld
        :accessor cld
        :initform nil)
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
   (author :initarg :author
           :accessor author
           :initform nil)
   (maintainer :initarg :maintainer
               :accessor maintainer
               :initform nil)
   (licence :initarg :licence
            :accessor licence
            :initform nil
            :type string)
   (creation-time :initarg :creation-time
                  :accessor creation-time
                  :initform (now))))

(defmethod print-object ((library library) stream)
  (print-unreadable-object (library stream :type t :identity t)
    (format stream "~A" (name library))))

(defun load-library (doc)
  (make-instance 'library 
		 :id (get-element "_id" doc)
		 :uuid (get-element "uuid" doc)
		 :name (get-element "name" doc)
		 :description (get-element "description" doc)
		 :cld (cldm::parse-cld-address 
		       (read-from-string (get-element "cld" doc)))
		 :logo (get-element "logo" doc)
		 :categories (get-element "categories" doc)
		 :keywords (get-element "keywords" doc)
		 :licence (get-element "licence" doc)
		 :author (get-element "author" doc)
		 :maintainer (get-element "maintainer" doc)
		 :doc doc))

(defun save-library (library)
  (let ((doc (or (doc library)
		 (make-document))))
    (add-element "name" (name library) doc)
    (add-element "description" (description library) doc)
    (add-element "cld" (format nil "~S" (cldm::unparse-cld-address (cld library)))
		 doc)
    (add-element "logo" (logo library) doc)
    (add-element "categories" (categories library) doc)
    (add-element "keywords" (keywords library) doc)
    (add-element "licence" (licence library) doc)
    (add-element "author" (author library) doc)
    (add-element "maintainer" (maintainer library) doc)
    (if (doc library)
	(db.save "libraries" doc)
	(progn
	  (add-element "uuid" (uuid library) doc)
	  (db.insert "libraries" doc)
	  (setf (doc library) doc)))))

(defmethod store ((library library))
  (save-library library))

(defun list-all-libraries ()
  (mapcar #'load-library (docs (db.find "libraries" :all))))

(defun find-library (uuid)
  (awhen (first (docs (db.find "libraries" (kv "uuid" uuid))))
    (load-library it)))

(defun find-library-by-name (name)
  (awhen (first (docs (db.find "libraries" (kv "name" name))))
    (load-library it)))  

(defun library-versions (library)
  (let ((versions
	 (mapcar #'load-library-version 
		 (docs (db.find "versions" 
				(kv "libraryuuid" (uuid library)))))))
    (sort versions
	  #'semver:version<
	  :key #'version)))

(defclass library-version (mongo-model)
  ((library :initarg :library
	    :accessor library
	    :initform nil)
   (version :initarg :version
            :accessor version
            :initform nil)
   (description :initarg :description
                :accessor description
                :initform nil
                :type string)
   (stability :initarg :stability
              :accessor stability
              :initform nil)
   (creation-time :initarg :creation-time
                  :accessor creation-time
                  :initform (now))
   (repositories :initarg :repositories
                 :initform (error "Provide a repository at least")
                 :accessor repositories
                 :documentation "Library version repositories")
   (dependencies :initarg :dependencies
                 :initform nil
                 :accessor dependencies
                 :documentation "The library version dependencies (list of requirement objects)"
		 :type list)
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

(defun save-library-version (library-version)
  (let ((doc (or (doc library-version)
		 (make-document))))
    (add-element "version" (cldm::print-version-to-string (version library-version)) doc)
    (add-element "libraryid" (id (library library-version)) doc)
    (add-element "libraryuuid" (uuid (library library-version)) doc)
    (add-element "stability" (stability library-version) doc)
    (add-element "description" (description library-version) doc)
    (add-element "repositories" (mapcar #'encode-repository (repositories library-version)) doc)
    (add-element "dependencies" (mapcar #'encode-requirement (dependencies library-version)) doc)
    (if (doc library-version)
	(db.save "versions" doc)
	(progn
	  (db.insert "versions" doc)
	  (setf (doc library-version) doc)))))

(defmethod store ((library-version library-version))
  (save-library-version library-version))

(defun load-library-version (doc)
  (make-instance 'library-version 
		 :id (get-element "_id" doc)
		 :uuid (get-element "uuid" doc)
		 :library (find-library (get-element "libraryuuid" doc))
		 :stability (get-element "stability" doc)
		 :description (get-element "description" doc)
		 :repositories (mapcar #'decode-repository 
				       (get-element "repositories" doc))
		 :dependencies (mapcar #'cldm::read-requirement-from-string
				       (get-element "dependencies" doc))
		 :doc doc))

(defun encode-repository (repository)
  (let ((repo (make-document)))
    (add-element "name" (cldm::name repository) repo)
    (add-element "address" 
		 (format nil "~S" 
			 (cldm::unparse-repository-address 
			  (cldm::repository-address repository)))
		 repo)
    repo))

(defun decode-repository (doc)
  (make-instance 'cldm::library-version-repository
		 :name (get-element "name" doc)
		 :address (cldm::parse-version-repository-address 
			   (read-from-string (get-element "address" doc)))))

(defun encode-requirement (requirement)
  (cldm::print-requirement-to-string requirement))

(defun publish (cldm-library)
  (let ((stored-library (find-library-by-name (cldm::library-name cldm-library))))
    (if stored-library
	(let ((cldm-library-version (first (cldm::library-versions cldm-library)))
	      (library-versions (library-versions stored-library)))	    
	       
	  ;; If the library version to publish already exists, or is inferior
	  ;; to the currently published one, error
	  (when (or (member (cldm::version cldm-library-version) library-versions
			    :key #'version
			    :test #'semver:version=)
		    (semver:version< (cldm::version cldm-library-version)
				     (version (first library-versions))))
	    (error "Invalid version ~A" cldm-library-version))
	  ;; Update the current library
	  (setf (description stored-library)
		(cldm::library-description cldm-library))
	  (setf (cld stored-library) (cldm::library-cld cldm-library))
	  (setf (licence stored-library) (cldm::library-licence cldm-library))
	  (setf (keywords stored-library) (cldm::library-keywords cldm-library))
	  (let ((library-version (make-instance 'library-version
						:version (cldm::version cldm-library-version)
						:description (cldm::description cldm-library-version)
						:stability (cldm::stability cldm-library-version)
						:dependencies (cldm::dependencies cldm-library-version)
						:library stored-library)))
	    (save-library stored-library)
	    (save-library-version library-version)))
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
						 :description (cldm::description cldm-library-version)
						 :stability (cldm::stability cldm-library-version)
						 :version (cldm::version cldm-library-version)
						 :repositories (cldm::repositories cldm-library-version)
						 :dependencies (cldm::dependencies cldm-library-version))))
	    (store library-version))))))
				 
			 
