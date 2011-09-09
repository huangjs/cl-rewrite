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
  (if *verbose-p*
      (print-unreadable-object (variable stream :type t :identity t)
        (format stream "~a => ~a" (name-of variable) (binding-of variable)))
      (print-unreadable-object (variable stream)
        (format stream "~a" (name-of variable)))))

(defgeneric copy-template-variable (variable))
(defgeneric template-variable-matches-p (variable value))
(defgeneric bind-template-variable (variable value)
  (:documentation "NOTE: Destructive"))
(defgeneric template-variable-boundp (variable))
(defgeneric instantiate-template-variable (variable))

(defmethod copy-template-variable ((variable template-variable))
  (copy-instance variable))

(defmethod template-variable-matches-p ((variable template-variable) value)
  (declare (ignorable variable value))
  t)

(defmethod bind-template-variable ((variable template-variable) value)
  (when (and *rebind-warning-p* (template-variable-boundp variable))
    (simple-style-warning "Re-apply value to template variable: ~a" (name-of variable)))
  (setf (binding-of variable) value))

(defmethod template-variable-boundp ((variable template-variable))
  (not (eq (binding-of variable) 'unbound)))

(defmethod instantiate-template-variable ((variable template-variable))
  (binding-of variable))

;;;; template variable with concept
(deftype concept () 'symbol)

(defclass template-variable-with-concept (template-variable)
  ((concept :initarg :concept
            :initform nil
            :accessor concept-of)))

(defmethod print-object ((variable template-variable-with-concept) stream)
  (if *verbose-p*
      (print-unreadable-object (variable stream :type t :identity t)
        (format stream "~a => ~a, concept: ~a"
                (name-of variable)
                (binding-of variable)
                (concept-of variable)))
      (call-next-method)))

