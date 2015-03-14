(in-package :cldm-registry.model)

(defparameter +mongo+ "cldm-registry")
(defparameter +users+ "users")
(defparameter +libraries+ "libraries")
(defparameter +versions+ "versions")
(defparameter +search-index-path+ 
  (asdf:system-relative-pathname :cldm-registry "searchidx/"))

(defparameter *search-index* 
  (make-instance 'montezuma:index
		 :path +search-index-path+))

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

(defun remove-directory (directory)
  (when (fad:directory-exists-p directory)
    (multiple-value-bind (output error status)
        (trivial-shell:shell-command (format nil "rm -r ~A" directory))
      (declare (ignore output))
      (when (not (zerop status))
        (error error)))))

(defun rebuild-search-index ()
  (remove-directory +search-index-path+)
  (setf *search-index* 
	(make-instance 'montezuma:index
		       :path +search-index-path+))  
  (loop for library in (list-all-libraries)
     do
       (montezuma:add-document-to-index *search-index*
					(build-search-document library))))

(defun search-library (term)
  (let ((search-result
	 (montezuma:search *search-index* term)))
    (loop for doc in (montezuma::score-docs search-result)
       collect 
	 (let ((docid (montezuma:doc doc))
	       (score (montezuma:score doc)))
	   (list (cons :library
		       (find-library-by-name
			(montezuma:document-value 
			 (montezuma:get-document *search-index*
						 docid)
			 "name")))
		 (cons :score score))))))
