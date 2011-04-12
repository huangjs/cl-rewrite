;; -*- Mode: common-lisp; Package: cl-user -*-
 
 (eval-when (:compile-toplevel)
 (error "This file is not intended to be compiled!"))
 
 #+nil
 ;;; Enable this if necessary
 (eval-when (:load-toplevel :execute)
 (setf *default-pathname-defaults* (make-pathname :directory (pathname-directory *load-truename*))))
 
 #+sbcl
 (require :asdf)
 #+(or allegro lispworks)
 (eval-when (:load-toplevel :execute)
 #+allegro (compile-file "lib/asdf/asdf" :if-newer t)
 #+lispworks (compile-file "lib/asdf/asdf")
 (load "lib/asdf/asdf"))
 
 (eval-when (:load-toplevel :execute)
 (setf asdf::*user-cache* 
 (merge-pathnames
 (concatenate 'string "fasl/"
 #+allegro "allegrocl/"
 #+lispworks "lispworks/"
 #+sbcl "sbcl/")
 *load-pathname*)))
 
 (cl:eval-when (:load-toplevel :execute)
 ;; allow load packages in lib/
 (loop for dir in (directory "lib/*/*/*.asd") do 
 (pushnew (make-pathname :directory (pathname-directory dir)) asdf:*central-registry* :test 'equal))
 (loop for dir in (directory "lib/*/*.asd") do 
 (pushnew (make-pathname :directory (pathname-directory dir)) asdf:*central-registry* :test 'equal))
 ;; enable/disable verifications, need recompilation
 (pushnew :verify *features*))
 
 ;; TODO:
 #+sbcl
 (setf sb-impl::*default-external-format* :utf-8)
 
 (asdf:oos 'asdf:load-op :iterate)
 (setf iterate::*always-declare-variables* t)
 
 (load ".asd")
 (asdf:oos 'asdf:load-op :)
 
 (setf *random-state* (make-random-state t))
 (setf *print-circle* t)
 
 ;; default configurations

