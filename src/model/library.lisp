(in-package :cldm-registry.model)

(defun create-library (&key name
			 description
			 author
			 maintainer
			 homepage
			 licence
			 keywords
			 categories)
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

    (db.insert "libraries" library)))

(defun list-all-libraries ()
  (docs (db.find "libraries" :all)))

(defun find-library (uuid)
  (first (docs (db.find "libraries" (kv "uuid" uuid)))))

(defun find-library-by-name (name)
  (first (docs (db.find "libraries" (kv "name" name)))))
			 
