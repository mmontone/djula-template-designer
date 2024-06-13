(defpackage :template-designer
  (:use :cl :cl-who :arrows)
  (:export #:start #:stop))

(in-package :template-designer)

(defparameter *templates-directory* nil)
(defvar *project-name*)
(defparameter *project-directory* nil)
(defparameter *config-directory* nil)
(defparameter *template-files-pattern* uiop/pathname:*wild-file-for-directory*
  "Pattern for listing the template files from the templates directory.
By default, all files are listed.
Example value: *.html")
(defparameter *assets-directory* nil)
(defparameter *templates* (make-hash-table :test 'equalp))

(defun project-directory ()
  (ensure-directories-exist
   (or *project-directory*
       (merge-pathnames (uiop/pathname:ensure-directory-pathname *project-name*)
                        *default-pathname-defaults*))))

(defun assets-directory ()
  (ensure-directories-exist
   (or *assets-directory*
       (merge-pathnames "assets/" (project-directory)))))

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
  ((id :initarg :id
       :accessor template-id
       :type string)
   (filename :initarg :filename
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

(defun list-template-files ()
  (uiop/filesystem:directory-files (templates-directory)
                                   *template-files-pattern*))

(defun register-template (template)
  (setf (gethash (template-id template) *templates*) template)
  template)

(defun make-template-id (filepath)
  (base64:string-to-base64-string
   (map 'string #'code-char (sha1:sha1-digest (princ-to-string filepath)))
   :uri t))

(defun load-templates ()
  (mapcar (lambda (filepath)
            (-> (make-instance 'template
                              :id (make-template-id filepath)
                              :filename (file-namestring filepath))
               (load-template)
               (register-template)))
          (list-template-files)))

(defun find-template (id)
  (find-if (lambda (template) (string= (template-id template) id))
           (load-templates)))

(defun condition-message (condition)
  "Get the descriptive message of CONDITION."
  (with-output-to-string (s)
    (write condition :escape nil :stream s)))

(defparameter +template-designer.js+ (asdf:system-relative-pathname :template-designer "template-designer.js"))

(defun with-site-html (stream body)
  (with-html-output (stream)
    (write-string "<!doctype html>" stream)
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
      (funcall body)))))

(defun render-navigation-bar (html)
  (with-html-output (html)
    (:nav :class "navbar is-dark"
          (:div :class "navbar-menu"
                (:div :class "navbar-start"
                      (:a :class "navbar-item"
                          :href "/"
                          (str "Templates"))
                      (:a :class "navbar-item"
                          :href "/settings"
                          (str "Settings"))
                      (:a :class "navbar-item"
                          :href "/help"
                          (str "Help")))))))

(defun render-main-page (destination &optional template)
  (uiop:with-output (html destination)
    (with-site-html html
      (lambda ()
        (with-html-output (html)
          (render-navigation-bar html)
          ;; templates form
          (:form :action "template" :method :post
                 (:div :class "fixed-grid has-4-cols"
                       (:div :class "grid"
                             (:div :class "cell"
                                   (:section :class "section"
                                             (:div :class "container"
                                                   (:h1 (str "Templates"))
                                                   (:select :size 5 :style "width: 100%;"
                                                            :onchange
                                                            (ps:ps-inline (redirect-to-template this))
                                                            (dolist (tmpl (load-templates))
                                                              (htm (:option :value (template-id tmpl)
                                                                            :selected (and template (string= (template-id tmpl) (template-id template)))
                                                                            (str (template-filename tmpl))
                                                                            ))))

                                                   (render-template-form template html)
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
                                             :href (format nil "/render?name=~a&id=~a" (template-filename template) (template-id template))
                                             :target "_blank" (str "Open in new tab")))
                                    (:iframe :width "100%" :style "border: 1px solid gray; width 100vw; height:100vh"
                                             :src (format nil "/render?name=~a&id=~a" (template-filename template) (template-id template))))))))
          (:script :type "text/javascript"
                   (str (alexandria:read-file-into-string +template-designer.js+)))
          (:script :type "text/javascript"
                   (str
                    (ps:ps
                      (defun redirect-to-template (elem)
                        (let* ((selected-index (ps:getprop elem 'selected-index))
                               (selected-option (aref (ps:getprop elem 'options) selected-index)))
                          (setf
                           (ps:chain window location href)
                           (concatenate
                            'string
                            "/?"
                            (ps:chain
                             (ps:new (-U-R-L-Search-Params (ps:create "id" (ps:getprop selected-option 'value)
                                                                      "name" (ps:getprop selected-option 'text))))
                             (to-string)))))))))
          )))))

(hunchentoot:define-easy-handler (main-handler :uri "/")
    (id)
  (render-main-page nil (find-template id)))

(defun render-template-form (template out)
  (with-html-output (out)
    (when template
      (htm
       (:input :type "hidden"
               :name "id"
               :value (template-id template))))
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

(hunchentoot:define-easy-handler (template-handler :uri "/template")
    ()
  (cond
    ((hunchentoot:post-parameter "save")
     (when (str:blankp (hunchentoot:post-parameter "filename"))
       (hunchentoot:redirect "/"))
     ;; Save the template file
     (let ((filepath (merge-pathnames (file-namestring (hunchentoot:post-parameter "filename")) (templates-directory))))
       (with-open-file (f filepath :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
         (write-string (hunchentoot:post-parameter "source") f))
       ;; Save the template configuration
       (let ((template-config (merge-pathnames (file-namestring (hunchentoot:post-parameter "filename")) (config-directory))))
         (with-open-file (f template-config :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede)
           (prin1 (hunchentoot:post-parameters*) f)))
       (hunchentoot:redirect (format nil "/?name=~a&id=~a" (hunchentoot:post-parameter "filename") (make-template-id filepath)))))
    ((hunchentoot:post-parameter "delete")
     (uiop/filesystem:delete-file-if-exists (merge-pathnames (file-namestring (hunchentoot:post-parameter "filename")) (templates-directory)))
     (hunchentoot:redirect "/"))
    ((hunchentoot:post-parameter "reload")
     (hunchentoot:redirect (format nil "/?name=~a&id=~a" (hunchentoot:post-parameter "filename") (hunchentoot:post-parameter "id"))))))

(hunchentoot:define-easy-handler (render-template :uri "/render")
    (id)
  (let ((template (or (find-template id)
                      (error "Template not found: ~s" id))))
    (handler-case
        (let ((template-args
                ;; To obtain the arguments for the template, first option is to read them from HTTP url if it is present
                (cond
                  ((not (str:blankp (template-data-url template)))
                   (handler-case
                       (multiple-value-bind (content status headers)
                           (drakma:http-request (template-data-url template))
                         (cond
                           ((>= status 400)
                            (error "Cannot access url: ~a" (template-data-url template)))
                           ((str:containsp "lisp" (access:access headers :content-type))
                            (read-from-string content))
                           ((str:containsp "json" (access:access headers :content))
                            (json:decode-json-from-source content))
                           (t
                            (error "Cannot resolve data-url content type"))))
                     (error (e)
                       (return-from render-template
                         (condition-message e)))))
                  ((not (str:blankp (template-arguments template)))
                   (handler-case (alexandria:alist-plist (read-from-string (template-arguments template)))
                     (error (read-error)
                       (handler-case
                           (alexandria:alist-plist (json:decode-json-from-string (template-arguments template)))
                         (error (json-error)
                           (return-from render-template
                             (who:with-html-output-to-string (html)
                               (:h3 :style "color:red;" (str "Error reading template arguments"))
                               (:p (:b (str "Reading as Lisp expression error: "))
                                   (str (condition-message read-error)))
                               (:p (:b (str "Reading as Json error: "))
                                   (str (condition-message json-error)))))))))))))
          (apply #'djula:render-template* (merge-pathnames (template-filename template) (templates-directory))
                 nil
                 template-args))
      (error (e)
        (write-to-string e :escape nil)))))

(defun render-settings-form (out)
  (macrolet ((text-input (name label value &rest args)
               `(with-html-output (out)
                  (:div :class "field is-small"
                        (:label :class "label is-small" (str ,label))
                        (:div :class "control"
                              (:input :name ,name
                                      :class "input is-small"
                                      :type "text"
                                      :value ,value
                                      ,@args))))))
    (with-html-output (out)
      (:form :method "post"
             (text-input "project-name" "Project name" *project-name* :readonly t)
             (text-input "project-directory" "Project directory" (princ-to-string (project-directory)) :readonly t)
             (text-input "templates-directory" "Templates directory" (princ-to-string (templates-directory)) :readonly t)
             (text-input "assets-directory" "Assets directory" (assets-directory) :readonly t)
             (text-input "config-directory" "Config directory" (princ-to-string (config-directory)) :readonly t)
             (text-input "template-files-pattern" "Templates file pattern" *template-files-pattern*)
             (:div :class "field"
                   (:label :class "checkbox"
                           (:input :name "debug-mode"
                                   :type "checkbox"
                                   :checked djula:*debug-mode*)
                           (str "Templates debug mode"))
                   (:p :class "help" (str "Display a panel with information about the rendered template.")))
             (:div :class "field"
                   (:label :class "checkbox"
                           (:input :name "strict-mode"
                                   :type "checkbox"
                                   :checked djula:*strict-mode*)
                           (str "Templates strict mode"))
                   (:p :class "help" (str "Signal template errors when trying to access an unbound variable.")))
             (:div :class "field is-grouped"
                   (:div :class "control"
                         (:button :class "button is-primary is-small"
                                  :type "submit"
                                  :name "update"
                                  (str "Update"))))))))

(hunchentoot:define-easy-handler (settings-handler :uri "/settings")
    ()
  (case (hunchentoot:request-method*)
    (:get
     (with-html-output-to-string (html)
       (with-site-html html
         (lambda ()
           (render-navigation-bar html)
           (htm (:div :class "container"
                      (render-settings-form html)))))))
    (:post
     (setf djula:*debug-mode* (hunchentoot:post-parameter "debug-mode"))
     (setf djula:*strict-mode* (hunchentoot:post-parameter "strict-mode"))
     (setf *template-files-pattern* (hunchentoot:post-parameter "template-files-pattern"))
     (hunchentoot:redirect "/settings"))))

(defparameter *djula-docs-url* "http://mmontone.github.io/djula/djula/")

(hunchentoot:define-easy-handler (help-handler :uri "/help")
    ()
  (hunchentoot:redirect *djula-docs-url*))

(defvar *acceptor*)

(defun start (project-name &key (port 0)
                             project-directory
                             config-directory
                             templates-directory
                             (open-browser t)
                             assets-directory)
  (setf *project-name* project-name)
  (setf *project-directory* project-directory)
  (setf *config-directory* config-directory)
  (setf *templates-directory* templates-directory)
  (setf *assets-directory* assets-directory)
  (djula:add-template-directory (templates-directory))
  (push (hunchentoot:create-folder-dispatcher-and-handler "/assets/" (assets-directory))
        hunchentoot:*dispatch-table*)
  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
  (when open-browser
    (trivial-open-browser:open-browser (format nil "http://localhost:~a" (hunchentoot:acceptor-port *acceptor*))))
  *acceptor*)

(defun stop ()
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil))

;; To start some project:
;; (start "my-project")

;; Demo using Djula demo files:
;; (start "djula-demo" :templates-directory (asdf:system-relative-pathname :djula "demo/templates/"))

;; Stop the template designer:
;; (stop)
