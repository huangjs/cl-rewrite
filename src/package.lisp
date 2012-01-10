;; -*- Mode: common-lisp; Package: cl-user -*-

(in-package :cl-user)

(defpackage :cl-rewrite
  (:use :cl :alexandria)
  (:export #:match
           #:transf-1
           #:transf
           #:transf-all
           #:defun/t
           #:defun/t-inline
           #:deftemplate
           #:static-eval
           #:static-if
           #:static-cond
           #:static-case
           #:static-inline
           #:defstruct/v 
           ))

