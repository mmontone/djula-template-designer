(defpackage :template-designer-demo
  (:use :cl)
  (:export :start))

(in-package :template-designer-demo)

(defvar *acceptor*)

(defun start ()
  (template-designer:start "template-designer-demo"
                           :project-directory (asdf:system-relative-pathname :template-designer "template-designer-demo/"))
  (setf *acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 9090 :name 'template-designer-demo))))

(hunchentoot:define-easy-handler (lisp-data :uri "/lisp-data" :acceptor-names '(template-designer-demo))
    ()
  (setf (hunchentoot:content-type*) "text/lisp")
  (prin1-to-string
   `((:lisp-implementation-version . ,(lisp-implementation-version))
     (:lisp-implementation-type . ,(lisp-implementation-type))
     (:packages . ,(mapcar #'package-name (list-all-packages))))))

(hunchentoot:define-easy-handler (json-data :uri "/json-data"
                                            :acceptor-names '(template-designer-demo))
    ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string
   `((:lisp-implementation-version . ,(lisp-implementation-version))
     (:lisp-implementation-type . ,(lisp-implementation-type))
     (:packages . ,(mapcar #'package-name (list-all-packages))))))
