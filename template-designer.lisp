(require :hunchentoot)
(require :djula)
(require :cl-who)
(require :easy-routes)
(require :trivial-open-browser)
(require :parenscript)
(require :cl-css)
(require :drakma)

(defpackage :template-designer
  (:use :cl :cl-who)
  (:export #:start))

(in-package :template-designer)

(defparameter *templates-directory* (user-homedir-pathname))

(defclass template ()
  ((filename :initarg :filename
             :accessor template-filename
             :type string)
   (rendering-engine :initarg :rendering-engine
                     :initform :djula)
   (data-url :initarg :data-url
             :initform nil
             :type (or null string)
             :accessor template-data-url)
   (arguments :initarg :arguments
              :initform nil
              :accessor template-arguments)))

(defun render-main-page (destination)
  (uiop:with-output (stream destination)
    (write-string "<!doctype html>" stream)
    (with-html-output (stream)
      (:html
       (:head
        (:title "Template designer")
        (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1"))
       (:body
        (:script :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.34.2/ace.js" :integrity "sha512-WdJDvPkK4mLIW1kpkWRd7dFtAF6Z0xnfD3XbfrNsK2/f36vMNGt/44iqYQuliJZwCFw32CrxDRh2hpM2TJS1Ew==" :crossorigin "anonymous" :referrerpolicy "no-referrer")
        (:script :src "https://unpkg.com/htmx.org@1.9.12")
        (:div :class "fixed-grid has-4-cols"
              (:div :class "grid"
                    (:div :class "cell"
                          (:section :class "section"
                                    (:div :class "container"
                                          (:h1 (str "Templates"))
                                          (:select :size 5
                                            (:option (str "main.html"))
                                            (:option (str "body.html"))
                                            (:option (str "test.html")))
                                          (render-template-form nil stream))))
                    (:div :class "cell is-col-span-3"
                          (:section :class "section"
                                    (:div :class "container"
                                          (:h1 (str "Template source"))
                                          (:div :id "editor"
                                                :style (cl-css:inline-css '(:width "100%" :height "400px"))
                                                (str "<html></html>")))))
                    (:div :class "cell is-col-span-4"
                          (:section :class "section"
                                    (:div :class "container"
                                          (:h1 (str "Rendered template")))))))

        (:script
         (str
          (ps:ps
            (setf (ps:chain window onload)
                  (lambda ()
                    (let ((editor (ps:chain ace (edit "editor"))))
                      ;;(ps:chain editor (set-theme "ace/theme/monokai"))
                      (ps:chain editor session (set-mode "ace/mode/html"))))))))
        )))))

(hunchentoot:define-easy-handler (main :uri "/")
    ()
  (render-main-page nil))

(defun render-template-form (template out)
  (if (null template)
      (with-html-output (out)
        (:form :action "/template" :method "POST"
               (:div :class "field is-small"
                     (:label :class "is-small" (str "Filename"))
                     (:div :class "control"
                           (:input :class "input is-small" :type "text" :placeholder "The template filename")))
               (:div :class "field is-small"
                     (:label :class "is-small" (str "Data url"))
                     (:div :class "control"
                           (:input :class "input is-small" :type "text" :placeholder "The template data url")))
               (:div :class "field is-small"
                     (:label :class "is-small" (str "Arguments"))
                     (:div :class "control"
                           (:input :class "input is-small" :type "text" :placeholder "The template arguments")))

               (:div :class "control"
                     (:button :class "button is-primary"
                              :type "submit"
                              (str "Save")))))))

(defvar *acceptor*)

(defun start ()
  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 0)))
  (trivial-open-browser:open-browser (format nil "http://localhost:~a" (hunchentoot:acceptor-port *acceptor*))))

(defun stop ()
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil))

;; (start)
;; (stop)
