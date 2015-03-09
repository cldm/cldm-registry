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
