(in-package :cl-rewrite)

(defstruct (defstruct-definition (:conc-name struct-))
  name
  options
  slots)

(defstruct (defstruct-options (:conc-name do-))
  conc-name
  constructor
  copier
  include
  initial-offset
  named
  predicate
  printer
  type)

(defstruct (defstruct-slot-definition (:conc-name slot-))
  name
  initform
  type
  read-only)

(defun parse-defstruct-options (name-and-options)
  (let ((name-and-options (alexandria:ensure-list name-and-options))
        options)
    (destructuring-bind (name &rest %options) name-and-options
      (setf options
            (apply #'make-defstruct-options (apply #'concatenate 'list %options)))
      (values name options))))

(defun parse-defstruct-slot-definition (slot-definition)
  (let ((slot-definition (alexandria:ensure-list slot-definition)))
    (destructuring-bind (name &optional initform &key type read-only) slot-definition
      (make-defstruct-slot-definition :name name
                                      :initform initform
                                      :type type
                                      :read-only read-only))))

(defun expander-for-defstruct (name-and-options slot-descriptions)
  ())

(defmacro defstruct/v (name-and-options &rest slot-descriptions)
  (expander-for-defstruct name-and-options slot-descriptions))


;;; TODO:
;;; - conc-name-option
;;; - copier-option
;;; - include-option
;;; - initial-offset-option
;;; - named-option
;;; - predicate-option
;;; - printer-option
;;; - type-option

