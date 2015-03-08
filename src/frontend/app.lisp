(in-package :cldm-registry.frontend)

(defparameter +host+ "localhost")
(defparameter +port+ 8090)

(defparameter *html* nil)

(defun start (&optional (host +host+) (port +port+))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				    :address host
				    :port port)))

(defun call-with-frontend-common (function)
  (who:with-html-output-to-string (*html*)
    (:html
     (:head
      (:title "CLDM registry")
      (:meta :http-equiv "Content-Type"
	     :content "text/html; charset=UTF-8")
      (:link :rel "shortcut icon"
	     :href "static/favicon.ico")
      (:link :rel "stylesheet"
	     :href "static/style.css")
      (:link :rel "static/bower_components/bootstrap/dist/css/bootstrap.min.css")
      (:link :rel "static/bower_components/bootstrap/dist/css/bootstrap-theme.min.css")
      (:script :type "javascript"
	       :src "static/bower_components/jquery/dist/jquery.min.js")
      (:script :type "javascript" :src "static/bower_components/bootstrap/dist/js/bootstrap.min.js"))
     (:body
      (funcall function)))))

(defmacro with-html (&body body)
  `(who:with-html-output (*html*)
     ,@body))

(defmacro with-frontend-common (&body body)
  `(call-with-frontend-common (lambda () (with-html ,@body))))

(hunchentoot:define-easy-handler (index-handler :uri "/") ()
  (with-frontend-common
    (:h1 "Hello world")))
