(in-package :cldm-registry.model)

(defun create-category (&key name description)
  (let ((category (make-document)))
    (add-element "uuid" (princ-to-string (uuid:make-v4-uuid)) category)
    (add-element "name" name category)
    (add-element "description" description category)
    (db.insert "categories" category)))

(defun list-all-categories ()
  (docs (db.find "categories" :all)))

(defun find-category (uuid)
  (first (docs (db.find "categories" (kv "uuid" uuid)))))
