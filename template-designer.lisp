(require :hunchentoot)
(require :djula)
(require :cl-who)
(require :easy-routes)
(require :trivial-open-browser)
(require :parenscript)
(require :cl-css)
(require :drakma)
(require :cl-json)

(defpackage :template-designer
  (:use :cl :cl-who)
  (:export #:start #:stop))

(in-package :template-designer)

(defparameter *templates-directory* nil)
(defvar *project-name*)
(defparameter *project-directory* nil)
(defparameter *config-directory* nil)

(defun project-directory ()
  (ensure-directories-exist
   (or *project-directory*
       (merge-pathnames (UIOP/PATHNAME:ENSURE-DIRECTORY-PATHNAME *project-name*)
                        *default-pathname-defaults*))))

(defun templates-directory ()
  (ensure-directories-exist
   (cond
     ((and *templates-directory* (uiop/pathname:absolute-pathname-p *templates-directory*))
      *templates-directory*)
     ((and *templates-directory* (uiop/pathname:relative-pathname-p *templates-directory*))
      (merge-pathnames *templates-directory* (project-directory)))
     (t
      (merge-pathnames #p"templates/" (project-directory))))))

(defun config-directory ()
  (ensure-directories-exist
   (cond
     ((and *config-directory* (uiop/pathname:absolute-pathname-p *config-directory*))
      *config-directory*)
     ((and *config-directory* (uiop/pathname:relative-pathname-p *config-directory*))
      (merge-pathnames *config-directory* (project-directory)))
     (t
      (merge-pathnames #p"templates-config/" (project-directory))))))

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

(defun template-source (template)
  (alexandria:read-file-into-string (merge-pathnames (template-filename template) (templates-directory))))

(defun load-template (template)
  (let ((config-file (merge-pathnames (template-filename template) (config-directory))))
    (when (uiop:file-exists-p config-file)
      (let ((config (read-from-string (alexandria:read-file-into-string config-file))))
        (setf (template-arguments template) (cdr (assoc "arguments" config :test #'string=))
              (template-data-url template) (cdr (assoc "data-url" config :test #'string=))))))
  template)

(defun load-templates ()
  (mapcar (lambda (filepath)
            (load-template (make-instance 'template
                                          :filename (file-namestring filepath))))
          (uiop/filesystem:directory-files (templates-directory))))

(defun find-template (filename)
  (find-if (lambda (template) (string= (template-filename template) filename))
           (load-templates)))

(defun condition-message (condition)
  "Get the descriptive message of CONDITION."
  (with-output-to-string (s)
    (write condition :escape nil :stream s)))

(defparameter +template-designer.js+ (merge-pathnames "template-designer.js" *load-pathname*))

(defun render-main-page (destination &optional template)
  (uiop:with-output (stream destination)
    (write-string "<!doctype html>" stream)
    (with-html-output (stream)
      (:html 
       (:head
        (:title "Template designer")
        (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1"))
       (:body
        (:script :src "https://code.jquery.com/jquery-3.7.1.js"
                 :integrity"sha256-eKhayi8LEQwp4NKxN+CfCh+3qOVUtJn3QNZ0TciWLP4="
                 :crossorigin "anonymous")
        (:script :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.34.2/ace.js"
                 :integrity "sha512-WdJDvPkK4mLIW1kpkWRd7dFtAF6Z0xnfD3XbfrNsK2/f36vMNGt/44iqYQuliJZwCFw32CrxDRh2hpM2TJS1Ew=="
                 :crossorigin "anonymous" :referrerpolicy "no-referrer")
        (:script :src "https://unpkg.com/htmx.org@1.9.12")
        (:form :action "template" :method :post
               (:div :class "fixed-grid has-4-cols"
                     (:div :class "grid"
                           (:div :class "cell"
                                 (:section :class "section"
                                           (:div :class "container"
                                                 (:h1 (str "Templates"))
                                                 (:select :size 5 :style "width: 100%;"
                                                   :onchange "window.location.href = \"/?template=\" + this.options[this.selectedIndex].value;"
                                                   (dolist (tmpl (load-templates))
                                                     (htm (:option :value (template-filename tmpl)
                                                                   :selected (and template (string= (template-filename tmpl) (template-filename template)))
                                                                   (str (template-filename tmpl))
                                                                   ))))

                                                 (render-template-form template stream)
                                                 )))
                           (:div :class "cell is-col-span-3"
                                 (:section :class "section"
                                           (:div :class "container"
                                                 (:h1 (str "Template source"))
                                                 (:textarea :id "editor"
                                                            :class "ace"
                                                            :name "source"
                                                            :style (cl-css:inline-css '(:width "100%" :height "400px"))
                                                            :rows 50
                                                            :width "100%"
                                                            :height "105px"
                                                            (str (or (and template (template-source template)) "<html></html>")))))))
                     (when template
                       (htm (:div :class "cell is-col-span-4"
                                  (:h1 (str "Rendered template")
                                       (:a :class "button is-small" :style "margin-left:10px;"
                                           :href (format nil "/render?name=~a" (template-filename template))
                                           :target "_blank" (str "Open in new tab")))
                                  (:iframe :width "100%" :style "border: 1px solid gray; width 100vw; height:100vh"
                                           :src (format nil "/render?name=~a" (template-filename template))))))))
        (:script :type "text/javascript"
                 (str (alexandria:read-file-into-string +template-designer.js+)))
        )))))

(hunchentoot:define-easy-handler (main :uri "/")
    (template)
  (render-main-page nil (find-template template)))

(defun render-template-form (template out)
  (with-html-output (out)
    (:div :class "field is-small"
          (:label :class "label is-small" (str "Filename"))
          (:div :class "control"
                (:input :name "filename"
                        :class "input is-small"
                        :type "text"
                        :placeholder "mytemplate.html"
                        :value (when template (template-filename template)))))
    (:div :class "field is-small"
          (:label :class "label is-small" (str "Data url"))
          (:div :class "control"
                (:input :name "data-url"
                        :class "input is-small"
                        :type "text"
                        :placeholder "http://site/data.json"
                        :value (when template (template-data-url template)))))
    (:div :class "field is-small"
          (:label :class "label is-small" (str "Arguments"))
          (:div :class "control"
                (:textarea :name "arguments"
                           :class "textarea"
                           :placeholder "{\"arg1\": \"foo\", \"arg2\" : \"bar\"}"
                           :rows 5
                           :style (cl-css:inline-css '(:width "100%"))
                           (when template
                             (str (template-arguments template))))))
    (:div :class "field is-grouped"
          (:div :class "control"
                (:button :class "button is-primary is-small"
                         :type "submit"
                         :name "save"
                         (str "Save")))
          (:div :class "control"
                (:button :class "button is-warning is-small"
                         :type "submit"
                         :name "reload"
                         (str "Reload")))
          (:div :class "control"
                (:button :class "button is-danger is-small"
                         :type "submit"
                         :name "delete"
                         (str "Delete"))))

    ))

(hunchentoot:define-easy-handler (handle-template :uri "/template")
    ()
  (cond
    ((hunchentoot:post-parameter "save")
     ;; Save the template file
     (let ((filepath (merge-pathnames (file-namestring (hunchentoot:post-parameter "filename")) (templates-directory))))
       (with-open-file (f filepath :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
         (write-string (hunchentoot:post-parameter "source") f)))
     ;; Save the template configuration
     (let ((template-config (merge-pathnames (file-namestring (hunchentoot:post-parameter "filename")) (config-directory))))
       (with-open-file (f template-config :direction :output
                                          :if-does-not-exist :create
                                          :if-exists :supersede)
         (prin1 (hunchentoot:post-parameters*) f)))
     ;;(who:escape-string (prin1-to-string (hunchentoot:post-parameters*)))
     (hunchentoot:redirect (format nil "/?template=~a" (hunchentoot:post-parameter "filename"))))
    ((hunchentoot:post-parameter "delete")
     ;; FIXME: security problem:
     (uiop/filesystem:delete-file-if-exists (merge-pathnames (file-namestring (hunchentoot:post-parameter "filename")) (templates-directory)))
     (hunchentoot:redirect "/"))
    ((hunchentoot:post-parameter "reload")
     (hunchentoot:redirect (format nil "/?template=~a" (hunchentoot:post-parameter "filename"))))))

(hunchentoot:define-easy-handler (render-template :uri "/render")
    (name)
  (let ((template (or (find-template name)
                      (error "Template not found: ~s" name))))
    (handler-case
        (apply #'djula:render-template* (merge-pathnames (template-filename template) (templates-directory))
               nil
               (when (template-arguments template)
                 (alexandria:alist-plist (json:decode-json-from-string (template-arguments template)))))
      (error (e)
        (write-to-string e :escape nil)))))

(defvar *acceptor*)

(defun start (project-name &key (port 0)
                             project-directory
                             config-directory
                             templates-directory)
  (setf *project-name* project-name)
  (setf *project-directory* project-directory)
  (setf *config-directory* config-directory)
  (setf *templates-directory* templates-directory)
  (djula:add-template-directory (templates-directory))
  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
  (trivial-open-browser:open-browser (format nil "http://localhost:~a" (hunchentoot:acceptor-port *acceptor*))))

(defun stop ()
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil))

;; (start "djula-test")
;; (stop)
