(asdf:defsystem template-designer
  :depends-on (:hunchentoot :djula :cl-who :trivial-open-browser
               :parenscript :cl-css :drakma :cl-json
               :str :arrows :cl-sha1 :uiop
               :cl-markdown :safe-read)
  :components ((:file "template-designer")))
