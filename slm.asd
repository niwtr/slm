;;;; slm.asd

(asdf:defsystem #:slm
  :description "Stack Lisp Machine"
  :author "TR Niw niwtr@icloud.com"
  :license "Specify license here"
  :depends-on (#:excalibur)
  :serial t
  :components ((:file "package")
               (:file "aux")
               (:file "scheme-macros")
               (:file "compile1")
               (:file "compile2")
               (:file "compile3")
               (:file "slm")))

