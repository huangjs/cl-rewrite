;; -*- Mode: common-lisp; Package: cl-user -*-

(asdf:defsystem :cl-rewrite
  :description "Term rewriting library for Common Lisp"
  :author "Jianshi Huang"
  :version "0.1"
  :licence "LLGPL"
  :serial t
  ;; add new files to this list:
  :components
  ((:module src
    :components ((:file "package")
                 (:file "meta" :depends-on ("package"))
                 (:file "variables" :depends-on ("meta"))
                 (:file "utils" :depends-on ("variables"))
                 (:file "template-variable" :depends-on ("utils"))
                 (:file "template" :depends-on ("template-variable"))
                 (:file "generics" :depends-on ("template"))
                 )))
  :depends-on (:alexandria
               ;; :parse-declarations-1.0
               ))

