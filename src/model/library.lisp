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
			 
