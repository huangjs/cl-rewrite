(in-package :cl-rewrite)

;;;; protocol design
(defclass basic-template-variable ()
  ((name :initarg :name
         :initform (error "Must specify the name of a template variable")
         :accessor template-name-name
         :type symbol)))

(defgeneric substitute-template-variables (form template-variables)
  )

;;;; template variable with concept
(defclass template-variable-with-concept (basic-template-variable)
  ((concept :initarg :concept
            :initform nil
            :accessor template-variable-concept
            :type symbol)))


