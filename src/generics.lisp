(in-package :cl-rewrite)

(defvar *lambda-list-keywords* '(&optional &key &rest &aux))

(defvar *generic-functions* (make-hash-table))
(defvar *instantiated-functions* (make-hash-table))

(defun clear-instantiated-functions ()
  (clrhash *instantiated-functions*))

(defun clear-generic-functions ()
  (clrhash *generic-functions*)
  (clear-instantiated-functions))

;;;
(defun template-var-style-check (var)
  "Must be a symbol, but can't be in the CL or KEYWORD package"
  (assert (symbolp var)
          nil
          "Template variable must be a symbol, current name: ~a" var)
  (assert (not (eq (find-package var) (find-package :cl)))
          nil
          "Symbol of template variable: ~s is in package CL, which is not allowed"
          var)
  (assert (not (eq (find-package var) (find-package :keyword)))
          nil
          "Symbol of template variable: ~s is in package KEYWORD, which is not allowed"
          var)
  var)

(defun extract-types-and-vars (typed-lambda-list)
  "return (values types vars) of typed vars, does not remove duplicated vars"
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
        collect (if (listp (first arg))
                    (first (first arg))
                    (first arg)) into vars
        finally (return (values tvars vars))))

(defun extract-ordinary-lambda-list (typed-lambda-list types)
  "return ordinary lambda-list without template variables"
  (loop for arg in typed-lambda-list
        if (not (listp arg))
        collect arg
        else
        collect (if (not (member (second arg) types))
                    arg
                    (if (= (length arg) 2)
                        (first arg)
                        (cons (first arg) (rest (rest arg)))))))

(defun extract-name-and-return-type (name)
  (etypecase name
    (symbol (values name nil))
    (list (values (first name) (second name)))))

(defun generate-type-declaration (vars types)
  (assert (= (length vars) (length types)))
  (loop for var in vars
        for type in types
        collect `(type ,type ,var)))

(defun generate-function-name (name types return-type)
  (let* ((return-type (or return-type t))
         (types (mapcar #'(lambda (type)
                            (with-standard-io-syntax
                              (format nil "~a" type)))
                        (append types (list return-type)))))
    (apply #'symbolicate (interleave (cons name types) '-))))

(defmacro defun/t (name args &body body)
  (multiple-value-bind (name return-type)
      (extract-name-and-return-type name)
    (multiple-value-bind (types vars)
        (extract-template-vars-and-vars args) 
      (let* ((docstring (when (stringp (first body)) (list (first body))))
             (body (if docstring (rest body) body))
             (lambda-list (extract-ordinary-lambda-list args types))
             (template-vars (remove-duplicates (if return-type (cons return-type types) types))))
        `(progn
           (deftemplate ,name ,template-vars
             (defun ,name ,lambda-list
               ,@docstring
               (declare ,@(generate-type-declaration vars types))
               ,@body)))))))

;; FIXME:
(defun generate-specialized-function-definition (name template return-type template-vars types args docstring)
  (let* ((name (generate-function-name name types))
         (docstring (ensure-list docstring)) 
         (return-type (eval (first (substitute-variables `(',return-type) (template-bindings template))))))
    `(progn
       ,(unless (eq return-type t)
          `(declaim (ftype (function * ,return-type) ,name)))
       (defun ,name ,args
         ,@docstring
         ,@(instantiate-template template)))))

;; FIXME:
(defun instantiate-function-by-types (name template return-type template-vars types args docstring)
  (assert (= (length template-vars) (length types)))
  (let ((instantiated-types (gethash name *instantiated-functions*))
        (code (generate-specialized-function-definition name template return-type template-vars types args docstring)))
    (when *debug-p*
      (format t "Instantiating function: ~s with types: ~s~%code:~%~a"
              name types code))
    (eval code)
    (if (null instantiated-types)
        (setf (gethash name *instantiated-functions*)
              (list types))
        (pushnew types (gethash name *instantiated-functions*) :test 'equalp))))

;; FIXME:
(defun get-function-by-types (function-name types)
  (when (member types (gethash function-name *instantiated-functions*) :test 'equalp)
    (symbol-function (generate-function-name function-name types))))

(defmacro defun/t (name args &body body)
  (assert (or (symbolp name)
              (= 2 (length name))))
  (multiple-value-bind (name return-type)
      (etypecase name
        (symbol (values name t))
        (list (values (first name) (second name))))
    (multiple-value-bind (template-vars typed-vars)
        ;; template-vars might have duplications
        (extract-template-vars-and-vars args)
      (let* ((docstring (when (stringp (first body)) (first body)))
             (body (if docstring (rest body) body))
             (lambda-list (extract-ordinary-lambda-list args template-vars))
             (typed-body `((declare ,@(generate-type-declaration typed-vars template-vars))
                           ,@body))
             (template (make-new-template (remove-duplicates template-vars) typed-body)))
        (setf (gethash name *generic-functions*) template)
        (loop for types in (gethash name *instantiated-functions*)
              do (instantiate-function-by-types name template return-type template-vars types lambda-list docstring))
        (with-unique-names (env whole)
          `(defmacro ,name ,(append (list '&whole whole) lambda-list (list '&environment env))
             ,@(list docstring)
             (declare (ignorable ,@typed-vars))
             (let* ((template ',template)
                    (name ',name)
                    (return-type ',return-type)
                    (template-vars ',template-vars)
                    (typed-vars ',typed-vars)
                    (types (loop for var in typed-vars
                                 for type = (variable-type-from-env var ,env)
                                 unless type
                                 do (warn "Cannot get type information of var ~a" var)
                                 collect (or type t)))
                    (function-name (generate-function-name ',name types))
                    (args (rest ,whole)))
               (assert (= (length template-vars) (length typed-vars) (length types)))
               (let ((instantiated-types (gethash name *instantiated-functions*)))
                 (unless (and instantiated-types
                              (member types instantiated-types :test 'equalp))
                   (instantiate-function-by-types name template return-type template-vars types args ,docstring)))
               `(,function-name ,@args))))))))
