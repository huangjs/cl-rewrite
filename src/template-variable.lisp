(in-package :cl-rewrite)

;;;; protocol design
(defclass template-variable ()
  ((name :initarg :name
         :initform (error "Must specify the name of a template variable")
         :accessor name-of
         :type symbol)
   (binding :initarg :binding
            :initform 'unbound
            :accessor binding-of)))

(defmethod print-object ((variable template-variable) stream)
  (print-unreadable-object (variable stream :type t :identity t)
    (format stream "~a => ~a"
            (name-of variable)
            (binding-of variable))))

(defgeneric template-variable-matches-p (variable value))
(defgeneric bind-template-variable (variable value))
(defgeneric template-variable-boundp (variable))
(defgeneric instantiate-template-variable (variable))

(defmethod template-variable-matches-p ((variable template-variable) value)
  (declare (ignorable variable value))
  t)

(defmethod bind-template-variable ((variable template-variable) value)
  (setf (binding-of variable) value))

(defmethod template-variable-boundp ((variable template-variable))
  (not (eq (binding-of variable) 'unbound)))

(defmethod instantiate-template-variable ((variable template-variable))
  (name-of variable))

;;;; template variable with concept
(deftype concept () 'symbol)

(defclass template-variable-with-concept (template-variable)
  ((concept :initarg :concept
            :initform nil
            :accessor concept-of)))

