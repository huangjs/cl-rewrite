(in-package :cl-rewrite)

(defclass template ()
  ((template-vars :initarg :template-vars
                  :initform nil
                  :accessor template-vars-of)
   (body :initarg :body
         :initform nil
         :accessor body-of)))

(defmethod print-object ((template template) stream)
  (print-unreadable-object (template stream :type t :identity t)
    (format stream "~%Variables:~{~%~A~}~%---------------~%Body:~%~A"
            (template-vars-of template)
            (body-of template))))

(defgeneric partial-apply (variable value template))
(defgeneric instantiate-template (template))

(defmethod instantiate-template ((template template))
  (assert (every #'template-variable-boundp (template-vars-of template))
          nil
          "Not all template variables are bound for template:~%~A"
          template)
  (walk-tree ((body-of template) :node-var node)
    (when (typep node 'template-variable)
      (instantiate-template-variable node))))

