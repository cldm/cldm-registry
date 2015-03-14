(in-package :cldm-registry.frontend)

(defclass api-auth-route (routes:proxy-route) ())

(defmethod restas:process-route :around ((route api-auth-route) bindings)
  (let* ((api-token (hunchentoot:header-in* "Authentication"))
	 (*user* (model::find-user-by-api-token api-token)))
    (if *user*
	(call-next-method)
	(progn
	  (setf (hunchentoot:return-code*)  hunchentoot:+http-forbidden+)
	  nil))))

(defun @api-auth (route)
  (make-instance 'api-auth-route :target route))

(restas:define-route api/publish ("/api/publish" :method :post)
  (:decorators '@api-auth)
  (let ((data (hunchentoot:raw-post-data :force-text t))
	(force-p (hunchentoot:get-parameter "force"))
	(preview-p (hunchentoot:get-parameter "preview")))
    (let ((cld (model::read-library-from-string data)))
      (json:encode-json-to-string (list :published (get-universal-time))))))

(defun registry-search (q)
  (list (list (cons :name "cldm") (cons :score "10"))
	(list (cons :name "hunchentoot") (cons :score "5"))))

(restas:define-route api/search ("/api/search")
  (:decorators '@api-auth)
  (let ((q (hunchentoot:get-parameter "q")))
    (when (not q)
      (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
      "q parameter missing")
    (let ((result (registry-search q)))
      (json:encode-json-to-string result))))
