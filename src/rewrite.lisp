(in-package :cl-rewrite)

(defvar *lambda-list-keywords* '(&optional &key &rest &aux))

(defvar *templates* (make-hash-table))
(defvar *template-vars*)

(defvar *instantiated-functions* (make-hash-table))

(defun clear-instantiated-functions ()
  (clrhash *instantiated-functions*))

(defun clear-templates ()
  (clrhash *templates*)
  (clear-instantiated-functions))

;; TODO
(defun template-var-style-check (var)
  var)

(defun process-template-vars (typed-lambda-list)
  (loop for arg in typed-lambda-list
        unless (member arg *lambda-list-keywords*)
        when (and (listp arg) (second arg))
        collect (progn
                  (assert (symbolp (second arg))
                          nil
                          "Type must be a symbol, error form: ~a" arg)
                  (assert (eq *package* (symbol-package (second arg)))
                          nil
                          "Can't use keyword symbol as type variable, error form: ~a" arg)
                  (template-var-style-check (second arg)))
        into tvars
        and
        collect (first arg) into vars
        finally (progn
                  (assert (= (length (remove-duplicates tvars))
                             (length tvars))
                          nil
                          "Duplicated type vars, error form: ~a" typed-lambda-list)
                  (return (values tvars vars)))))

(defun process-lambda-list (typed-lambda-list template-vars)
  (loop for arg in typed-lambda-list
        if (not (listp arg))
        collect arg
        else
        collect (if (not (member (second arg) template-vars))
                    arg
                    (if (= (length arg) 2)
                        (first arg)
                        (cons (first arg) (rest (rest arg)))))))

(defun all-variables-from-lambda-list (lambda-list)
  (loop for arg in (loop for arg in lambda-list
                         unless (member arg *lambda-list-keywords*)
                         collect arg)
        if (not (listp arg))
        collect arg
        else
        collect (first arg)))

(defun process-body (body template-vars replacements)
  (assert (= (length template-vars) (length replacements)))
  (loop for var in template-vars
        for r in replacements
        do (setf body (subst r var body)))
  body)

(defun generate-type-declaration (vars types)
  (assert (= (length vars) (length types)))
  (loop for var in vars
        for type in types
        collect `(type ,type ,var)))

(defun make-function-name (name types)
  (apply #'symbolicate (interleave (cons name types) '-)))

(defmacro defun/t (name args &body body)
  (multiple-value-bind (*template-vars* typed-vars)
      (process-template-vars args)
    (let* ((lambda-list (process-lambda-list args *template-vars*))
           (instantiator (symbolicate 'instantiate-template '- name)))
      (setf (gethash name *templates*)
            (list :template-vars *template-vars*
                  :typed-vars typed-vars
                  :lambda-list lambda-list
                  :body body
                  :instantiator instantiator))
      (with-unique-names (env whole types)
        `(progn
           (defmacro ,instantiator (,types)
             (let* ((function-name (make-function-name ',name ,types))
                    (lambda-list ',lambda-list)
                    (template-vars ',*template-vars*)
                    (typed-vars ',typed-vars)
                    (body ',body))
               `(defun ,function-name ,lambda-list
                  (declare ,@(generate-type-declaration typed-vars ,types))
                  (locally
                      ,@(process-body body template-vars ,types)))))
           (defmacro ,name ,(append (list '&whole whole) lambda-list (list '&environment env))
             (declare (ignorable ,@typed-vars))
             (let* ((template (gethash ',name *templates*))
                    (name ',name)
                    (typed-vars (getf template :typed-vars))
                    (types (loop for var in typed-vars
                                 for type = (variable-type-from-env var ,env)
                                 unless type
                                 do (warn "Cannot get type information of var ~a" var)
                                 collect (or type t)))
                    (function-name (make-function-name name types))
                    (instantiator ',instantiator)
                    (args (rest ,whole)))
               (unless (gethash function-name *instantiated-functions*)
                 (eval `(,instantiator ,types))
                 (setf (gethash function-name *instantiated-functions*) t))
               `(,function-name ,@args))))))))

