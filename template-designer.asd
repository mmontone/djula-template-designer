(asdf:defsystem template-designer
  :depends-on (:hunchentoot :djula :cl-who :trivial-open-browser
               :parenscript :cl-css :drakma :cl-json)
  :components ((:file "template-designer")))
