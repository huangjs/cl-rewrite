;; -*- Mode: common-lisp; Package: cl-user -*-

(in-package :cl-user)

(defpackage :cl-rewrite
  (:use :cl :alexandria)
  (:export #:defun/t
           #:defun/t-inline
           #:deftemplate
           #:static-eval
           #:static-if
           #:static-cond
           #:static-case
           #:static-inline
           ))

