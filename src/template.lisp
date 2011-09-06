(in-package :cl-rewrite)

(defvar *templates* (make-hash-table))
(defvar *special-forms* '(static-eval static-if static-cond static-case))

(defun find-template (name)
  (gethash name *templates*))

(defsetf find-template (name) (new)
  `(setf (gethash ,name *templates*) ,new))

;;;; protocol design

(defclass template ()
  ((name :initarg :name
         :initform nil
         :accessor name-of)
   (template-vars :initarg :template-vars
                  :initform nil
                  :accessor template-vars-of)
   (body :initarg :body
         :initform nil
         :accessor body-of)))

(defmethod print-object ((template template) stream)
  (print-unreadable-object (template stream :type t)
    (format stream "~A~%---------------~%Variables:~{~%  ~A~}"
            (name-of template)
            (template-vars-of template))
    (let ((*verbose-p* nil))
      (format stream "~%---------------~%Body:~%~s~%"
              (body-of template)))))

(defgeneric copy-template (template))
(defgeneric find-template-variable (name template &key test))
(defgeneric instantiate-template (template))

(defmethod copy-template ((template template))
  (let* ((old (template-vars-of template))
         (new (mapcar #'copy-template-variable old)))
    (make-instance 'template
                   :name (name-of template)
                   :template-vars new
                   :body (walk-nodes ((body-of template) :copy-tree t :node-var node)
                           (if (typep node 'template-variable)
                               (nth (position node old) new)
                               node)))))

(defmethod find-template-variable (name (template template) &key test)
  (find name (template-vars-of template) :key #'name-of :test test))

(defmethod find-template-variable (name (template symbol) &key test)
  (find-template-variable name (find-template template) :test test))

(defmethod instantiate-template ((template template))
  (loop for v in (template-vars-of template)
        do (assert (template-variable-boundp v)
                   nil
                   "Template variable ~a is not bound. Template: ~%~A"
                   v
                   template)) 
  (let ((body (walk-subtree ((body-of template) :copy-tree t :subtree-var subtree)
                (let ((first (first subtree)))
                  (if (member first *special-forms*)
                      (eval subtree)
                      subtree)))))
    (walk-nodes (body :copy-tree t :node-var node)
      (if (typep node 'template-variable)
          (instantiate-template-variable node)
          node))))

;;;; special form
(defmacro static-eval (form)
  form)

(defmacro static-if (test then &optional else)
  `(if ,test ,then ,else))

(defmacro static-cond (&rest clauses)
  `(cond ,@clauses))

(defmacro static-case (keyform &body cases)
  `(case ,keyform
     ,@cases))

;;;; macros
(defmacro with-deftemplate-environment ((name-and-default-classes template-variables template-body) &body body)
  "contextual helper function"
  ;; default template class and template-variable class
  (once-only (name-and-default-classes template-variables template-body)
    `(multiple-value-bind (name template-class template-variable-class)
         (etypecase ,name-and-default-classes
           ;; name can be nil
           (symbol (values ,name-and-default-classes 'template 'template-variable))
           (list (values (first ,name-and-default-classes)
                         (or (second ,name-and-default-classes) 'template)
                         (or (third ,name-and-default-classes) 'template-variable))))
       ;; compute template-variable constructor form
       (labels ((compute-args-from-variable-form (var)
                  (etypecase var
                    (symbol (list template-variable-class :name var))
                    (list (multiple-value-bind (name class rest-args)
                              (if (keywordp (second var))
                                  (values (first var) template-variable-class (rest var))
                                  (values (first var) (or (second var) template-variable-class) (rest (rest var))))
                            (list* class :name name rest-args))))))
         ;; 
         (let* ((var-forms (mapcar #'compute-args-from-variable-form ,template-variables))
                (var-names (mapcar (lambda (var-form) (getf (rest var-form) :name)) var-forms))
                (vars (mapcar (lambda (var-form) (apply #'make-instance var-form)) var-forms))
                ;; replace symbol with template-variable object
                (body (walk-nodes (,template-body :copy-tree t :node-var node)
                        (if (member node var-names)
                            (find node vars :key #'name-of)
                            node))))
           (assert (= (length var-names) (length (remove-duplicates var-names)))
                   nil
                   "Duplicated template variables declaration in: ~a"
                   ,template-variables)
           (locally
               ,@body))))))

(defmacro deftemplate (name-and-default-classes variables &body body)
  "syntax for quick template definition"
  (with-deftemplate-environment (name-and-default-classes variables body)
    (make-instance template-class
                   :name name
                   :template-vars vars
                   :body body)))

(defmacro deftemplate* (name-and-default-classes variables &body body)
  "syntax for quick template definition, make it searchable"
  (with-deftemplate-environment (name-and-default-classes variables body)
    (when (find-template name)
      (simple-style-warning "Redefining template ~a" name))
    (setf (find-template name)
          (make-instance template-class
                         :name name
                         :template-vars vars
                         :body body))))

