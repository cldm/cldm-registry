(in-package :cldm-registry.model)


(defun create-library (&key name
			 description
			 author
			 maintainer
			 homepage
			 licence
			 keywords
			 categories
			 cld)
  (let ((library (make-document)))
    (add-element "uuid" (princ-to-string (uuid:make-v4-uuid)) library)
    (add-element "name" name library) 
    (add-element "description" description library)
    (add-element "homepage" homepage library)
    (add-element "author" author library)
    (add-element "maintainer" maintainer library)
    (add-element "licence" licence library)
    (add-element "keywords" keywords library)
    (add-element "categories" categories library)
    (add-element "cld" (cldm::unparse-cld-address cld) library)

    (db.insert "libraries" library)))

(defun list-all-libraries ()
  (docs (db.find "libraries" :all)))

(defun find-library (uuid)
  (first (docs (db.find "libraries" (kv "uuid" uuid)))))

(defun find-library-by-name (name)
  (first (docs (db.find "libraries" (kv "name" name)))))

(defun library-uuid (library)
  (get-element "uuid" library))

(defun library-version-version (library-version)
  (cldm::read-version-from-string (get-element "version" library-version)))

(defun library-versions (library)
  (let ((versions
	 (docs (db.find "versions" (kv "libraryuuid" (library-uuid library))))))
    (sort versions
	  #'semver:version<
	  :key #'library-version-version)))

(defun create-library-version (&key library
				 version
				 repositories)
  (let ((library-version (make-document)))
    (add-element "uuid" (princ-to-string (uuid:make-v4-uuid)) library-version)
    (add-element "version" (cldm::print-version-to-string library-version))
    (add-element "libraryid" (get-element "_id" library))
    (add-element "libraryuuid" (get-element "uuid" library))
    (add-element "repositories" (encode-repositories (cldm::repositories version)))
    (db.insert "versions" library-version)))

(defun encode-repositories (repositories)
  (loop for repository in repositories
       collect (encode-repository repository)))

(defun encode-repository (repository)
  (let ((repo (make-document)))
    (add-element "name" (cldm::name repository) repo)
    (add-element "address" (cldm::unparse (cldm::repository-address repository)))
    repo))


(defun publish (cldm-library)
  (let ((stored-library (find-library-by-name (cldm::library-name cldm-library))))
    (if stored-library
	(let ((cldm-library-version (first (cldm::library-versions cldm-library)))
	      (library-versions (library-versions stored-library)))
	  ;; If the library version to publish already exists, or is inferior
	  ;; to the currently published one, error
	  (if (or (member (cldm::version cldm-library-version) library-versions
			  :key #'library-version-version
			  :test #'semver:version=)
		  (semver:version< (cldm::version cldm-library-version)
				   (library-version-version (first library-versions))))
	      (error "Invalid version ~A" cldm-library-version))
	  ;; Update the current library
	  (add-element  (description stored-library)
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
				 
			 
