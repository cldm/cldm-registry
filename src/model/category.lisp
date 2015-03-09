(in-package :cldm-registry.model)

(def-view-class category ()
  ((id :accessor id 
       :initarg :id
       :initform (sequence-next "category-id-seq")
       :type integer 
       :db-kind :key 
       :db-constraints (:not-null))
   (name :initarg :name
	 :accessor name
	 :initform nil
	 :type string
	 :db-constraints (:not-null)
	 :db-kind :key)
   (description :initarg :description
	     :accessor description
	     :initform nil
	     :type string)))
