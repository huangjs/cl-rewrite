(in-package :cl-rewrite)

(defvar *templates* (make-hash-table))

(defstruct (template (:constructor %make-template))
  vars
  bindings
  body)

(defun make-new-template (vars body)
  (assert (every #'symbolp vars)
          nil
          "Template variable must be a symbol, current variables: ~s" vars)
  (assert (= (length vars) (length (remove-duplicates vars)))
          nil
          "Found duplicated template variables, current variables: ~s" vars) 
  (%make-template :vars vars :body body))

;;;
(defun substitute-variables (body bindings)
  (let ((body (list (copy-tree body))))
    (loop for (var binding) in bindings
          do (nsubst binding var body))
    (first body)))

(defun apply-substitution (template bindings)
  (let ((template (copy-template template)))
    (loop for b in bindings
          for (var binding) = b
          do (progn
               (assert (not (assoc var (template-bindings template)))
                       nil
                       "Variable ~s already bound"
                       var)
               (assert (member var (template-vars template))
                       nil
                       "Variable ~s not found in template" var)))
    (loop for b in bindings
          for (var binding) = b
          do (appendf (template-bindings template)
                      (list (list var (eval (substitute-variables binding bindings))))))
    template))

;;;
(defmacro tlambda (vars body)
  (make-new-template vars body))

(defmacro tapply (template &rest bindings)
  (once-only (template)
    `(let ((bindings ',bindings))
       (apply-substitution ,template bindings))))

;; TODO:
;; (defmacro teval (form)
;;   )

(defmacro deftemplate (name vars body &optional (templates *templates*))
  (assert (symbolp name)
          nil
          "Template name must be a symbol")
  (let ((template (make-new-template vars body)))
    (when (gethash name templates)
      (warn "Redefining template: ~s" name))
    (setf (gethash name templates) template)))

(defun get-template (name &optional errorp (templates *templates*))
  (let ((template (gethash name templates)))
    (when (and errorp (null template))
      (error "Cannot find template: ~s" name))
    template))

(defun instantiate-template (template &optional (templates *templates*))
  (let ((template (etypecase template
                    (symbol (get-template template t templates))
                    (template template))))
    (with-slots (vars bindings body) template
      (assert (and (= (length vars) (length bindings))
                   (loop for v in vars
                         always (assoc v bindings)))
              nil
              "Not all variables are bound for template: ~s" template)
      (substitute-variables body bindings))))

