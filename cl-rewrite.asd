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
            :components ((:file "packages")
                         (:file "utils" :depends-on ("packages"))
                         ;; (:file "template" :depends-on ("utils"))
                         ;; (:file "generics" :depends-on ("template"))
                         )))
  :depends-on (:alexandria
               ;; :parse-declarations-1.0
               ))

