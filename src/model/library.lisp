(in-package :cldm-registry.model)

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
   (homepage :initarg :homepage
	     :initform nil
	     :accessor homepage)
   (documentation :initarg :documentation
		  :initform nil
		  :accessor library-documentation)
   (bug-reports :initarg :bug-reports
		:initform nil
		:accessor bug-reports)
   (source-repository :initarg :source-repository
		      :initform nil
		      :accessor source-repository)   
   (publisher :initarg :publisher
	      :accessor publisher
	      :initform nil
	      :documentation "The cldm-registry user that published the library")
   (creation-time :initarg :creation-time
                  :accessor creation-time
                  :initform (now))
   (update-time :initarg :update-time
		:accessor update-time
		:initform (now))))

(defmethod print-object ((library library) stream)
  (print-unreadable-object (library stream :type t :identity t)
    (format stream "~A" (name library))))

(defun load-library (doc)
  (make-instance 'library 
		 :id (get-element :_id doc)
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
		 :homepage (get-element "homepage" doc)
		 :bug-reports (get-element "bug-reports" doc)
		 :documentation (get-element "documentation" doc)
		 :source-repository (get-element "source-repository" doc)
		 :publisher (find-user-by-id (get-element "publisher" doc))
		 :creation-time (get-element "creation-time" doc)
		 :update-time (get-element "update-time" doc)
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
    (add-element "publisher" (make-bson-oid (id (publisher library))) doc)
    (add-element "homepage" (homepage library) doc)
    (add-element "bug-reports" (bug-reports library) doc)
    (add-element "documentation" (library-documentation library) doc)
    (add-element "source-repository" (source-repository library) doc)
    (add-element "update-time" (now) doc)
    (if (doc library)
	(db.save "libraries" doc)
	(progn
	  (add-element "uuid" (uuid library) doc)
	  (add-element "creation-time" (now) doc)
	  (db.insert "libraries" doc)
	  (setf (doc library) doc)
	  (montezuma:add-document-to-index *search-index*
					   (build-search-document library))
	  library))))

(defun remove-library (library)
  (let ((library-versions (library-versions library)))
    (db.delete "libraries" (doc library))
    (loop for version in library-versions
	 do (db.delete "versions" (doc version)))))

(defmethod store ((library library))
  (save-library library))

(defun list-all-libraries ()
  (mapcar #'load-library (docs (db.find "libraries" :all))))

(defun find-library-by-id (id)
  (awhen (first (docs (db.find "libraries" (kv :_id (make-bson-oid id)))))
    (load-library it)))

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
				(kv "libraryuuid" (uuid library)) :limit 0)))))
    (sort versions
	  #'semver:version>
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
   (custom-repositories :initarg :custom-repositories
			:initform nil
			:accessor custom-repositories)
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

(defmethod print-object ((library-version library-version) stream)
  (print-unreadable-object (library-version stream :type t :identity t)
    (format stream "~A-~A" (name (library library-version))
	    (semver:print-version-to-string (version library-version)))))

(defun find-library-version (&rest args)
  (if (equalp (length args) 1)
      (db.find +versions+ (kv "uuid" (first args)))
      (destructuring-bind (name version) args
	(let ((library (find-library-by-name name))
	      (version (semver:read-version-from-string version)))
	  (find-if (lambda (v)
		     (semver:version= (version v)
				      version))
		   (library-versions library))))))

(defun save-library-version (library-version)
  (let ((doc (or (doc library-version)
		 (make-document))))
    (add-element "version" (cldm::print-version-to-string (version library-version)) doc)
    (add-element "libraryid" (id (library library-version)) doc)
    (add-element "libraryuuid" (uuid (library library-version)) doc)
    (add-element "stability" (stability library-version) doc)
    (add-element "description" (description library-version) doc)
    (add-element "repositories" (mapcar #'encode-repository (repositories library-version)) doc)
    (add-element "custom-repositories" (mapcar #'encode-repository (custom-repositories library-version)) doc)
    (add-element "dependencies" (mapcar #'encode-requirement (dependencies library-version)) doc)
    (add-element "creation-time" (creation-time library-version) doc)
    (if (doc library-version)
	(db.save "versions" doc)
	(progn
	  (db.insert "versions" doc)
	  (setf (doc library-version) doc)))
    ;; Add the library version to the published library versions collection
    (db.insert "published-library-versions" doc)
    library-version))

(defmethod store ((library-version library-version))
  (save-library-version library-version))

(defun load-library-version (doc)
  (make-instance 'library-version 
		 :id (get-element :_id doc)
		 :uuid (get-element "uuid" doc)
		 :library (find-library (get-element "libraryuuid" doc))
		 :version (semver:read-version-from-string (get-element "version" doc))
		 :stability (get-element "stability" doc)
		 :description (get-element "description" doc)
		 :repositories (mapcar #'decode-repository 
				       (get-element "repositories" doc))
		 :repositories (mapcar #'decode-repository 
				       (get-element "custom-repositories" doc))
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
		 :address (cldm::parse-repository-address 
			   (read-from-string (get-element "address" doc)))))

(defun encode-requirement (requirement)
  (cldm::print-requirement-to-string requirement))

(defun publish (cldm-library publisher)
  (let ((stored-library (find-library-by-name (cldm::library-name cldm-library))))
    (if stored-library
	(let ((cldm-library-version (first (cldm::library-versions cldm-library)))
	      (library-versions (library-versions stored-library)))	    
	       
	  ;; If the library version to publish already exists, or is inferior
	  ;; to the currently published one, error
	  (when (member (cldm::version cldm-library-version) library-versions
			    :key #'version
			    :test #'semver:version=)
	    (error "Version ~A of ~A already exists" 
		   (cldm::version cldm-library-version)
		   stored-library))
	  (when (semver:version< (cldm::version cldm-library-version)
				     (version (first library-versions)))
	    (error "Version is too old ~A" cldm-library-version))
	  ;; Update the current library
	  (setf (description stored-library)
		(cldm::library-description cldm-library))
	  (setf (cld stored-library) (cldm::library-cld cldm-library))
	  (setf (licence stored-library) (cldm::library-licence cldm-library))
	  (setf (keywords stored-library) (cldm::library-keywords cldm-library))
	  (setf (author stored-library) (cldm::library-author cldm-library))
	  (setf (maintainer stored-library) (cldm::library-maintainer cldm-library))
	  (setf (homepage stored-library) (cldm::library-homepage cldm-library))
	  (setf (library-documentation stored-library) (cldm::library-documentation cldm-library))
	  (setf (bug-reports stored-library) (cldm::library-bug-reports cldm-library))
	  (setf (source-repository stored-library) (cldm::library-source-repository cldm-library))
	  (let ((library-version (make-instance 'library-version
						:version (cldm::version cldm-library-version)
						:description (cldm::description cldm-library-version)
						:stability (cldm::stability cldm-library-version)
						:dependencies (cldm::dependencies cldm-library-version)
						:repositories (cldm::repositories cldm-library-version)
						:library stored-library)))
	    (store stored-library)
	    (store library-version)))
	;; else, create the library
	(let ((library (make-instance 'library 
				      :name (cldm::library-name cldm-library)
				      :cld (cldm::library-cld cldm-library)
				      :description (cldm::library-description cldm-library)
				      :keywords (cldm::library-keywords cldm-library)
				      :licence (cldm::library-licence cldm-library)
				      :author (cldm::library-author cldm-library)
				      :maintainer (cldm::library-maintainer cldm-library)
				      :homepage (cldm::library-homepage cldm-library)
				      :documentation (cldm::library-documentation cldm-library)
				      :bug-reports (cldm::library-bug-reports cldm-library)
				      :source-repository (cldm::library-source-repository cldm-library)
				      :publisher publisher)))
	  (store library)
	  (let* ((cldm-library-version (first (cldm::library-versions cldm-library)))
		 (library-version (make-instance 'library-version
						 :library library
						 :description (cldm::description cldm-library-version)
						 :stability (cldm::stability cldm-library-version)
						 :version (cldm::version cldm-library-version)
						 :repositories (cldm::repositories cldm-library-version)
						 :custom-repositories (cldm::custom-repositories cldm-library-version)
						 :dependencies (cldm::dependencies cldm-library-version))))
	    (store library-version))))))

(defun print-library-definition (library &optional stream)
  (let ((*print-case* :downcase))
    (format stream "~S" (library-definition library))))

(defun library-definition (library)
  `(cldm:deflibrary ,(intern (string-upcase (name library)))
     :cld ,(cldm::unparse-cld-address (cld library))
     :description ,(description library)
     :author ,(author library)
     :licence ,(licence library)
     :versions ,(mapcar #'library-version-definition (library-versions library))))

(defun library-version-definition (library-version)
  `(:version ,(semver:print-version-to-string (version library-version))
	     :repositories ,(mapcar #'cldm::unparse-library-version-repository
				    (repositories library-version))
	     :depends-on ,(mapcar #'cldm::print-requirement-to-string 
				  (dependencies library-version))))

(defun latest-library-versions (&key (limit 10))
  (db.sort "published-library-versions" :all 
	   :field "creation-time"
	   :limit limit))

(defun parse-library (librarydef)
  (destructuring-bind (deflibrary name &body options) librarydef
    (assert (or (equalp deflibrary 'cldm:deflibrary)
		(equalp deflibrary 'cl-user::deflibrary)))
    (destructuring-bind (&key author maintainer description
			      licence cld version keywords 
			      homepage documentation bug-reports source-repository
			      &allow-other-keys)
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
				  :homepage homepage
				  :documentation documentation
				  :bug-reports bug-reports
				  :source-repository source-repository
				  :keywords keywords
				  :versions nil))
		  (library-version (make-instance 'cldm::library-version
						  :version (cldm::read-version-from-string (getf options :version))
						  :stability (getf options :stability)
						  :repositories (cldm::parse-version-repositories (getf options :repositories))
						  :custom-repositories (cldm::parse-version-repositories (getf options :custom-repositories))
						  :dependencies (cldm::parse-version-dependencies (getf options :depends-on))
						  :suggests (cldm::parse-version-dependencies (getf options :suggests))
						  :provides (cldm::parse-version-dependencies (getf options :provides))
						  :conflicts (cldm::parse-version-dependencies (getf options :conflicts)))))
	      (setf (cldm::library library-version) library)
	      (setf (cldm::library-versions library)
		    (list library-version))
	      library))
	  ;; else, we assume it is the full version
	  (cldm::parse-library librarydef)))))

(cl-secure-read::define-secure-read-from-string secure-read-from-string
    :whitelist (append  (list #\; #\#) 
			cl-secure-read:safe-read-from-string-whitelist))

(defun read-library-from-string (str)
  (parse-library (secure-read-from-string str)))

(defun build-search-document (library)
  (list (cons "name" (model::name library))
        (cons "author" (model::author library))
	(cons "description" (model::description library))
	(cons "licence" (model::licence library))
	;; Montezuma doesnt support lists 
	;(cons "keywords" (model::keywords library))
	;(cons "categories" (model::categories library))
	))
