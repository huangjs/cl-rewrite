(in-package :cl-rewrite)

(defparameter *verbose-p* t)
(defparameter *rebind-warning-p* t)
(defparameter *debug-p* t)

(defvar *templates* (make-hash-table))
(defvar *special-forms* '(static-eval static-if static-cond static-case))

(declaim (type hash-table *templates*)
         (type list *special-forms*))

(defvar *generic-functions* (make-hash-table))
(defvar *instantiated-functions* (make-hash-table))

(declaim (type hash-table *generic-functions* *instantiated-functions*))

