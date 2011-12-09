;; -*- Mode: common-lisp; Package: cl-user -*-

(asdf:defsystem :cl-rewrite-test
  :author "Jianshi Huang"
  :serial t
  :version "0.1"
  :licence "LLGPL"
  ;; add new files to this list:
  :components
  ((:module test
    :components ((:file "package")
                 (:file "basic-tests")
                 (:file "test-program-transformation")
                 )
    :serial t))
  :depends-on (:cl-rewrite
               :stefil))

