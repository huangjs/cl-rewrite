(in-package :cl-rewrite)

(defvar *generic-functions* (make-hash-table))
(defvar *instantiated-functions* (make-hash-table))

(declaim (type hash-table *generic-functions* *instantiated-functions*))

(defun find-generic-function (name)
  (gethash name *generic-functions*))

(defsetf find-generic-function (name) (new)
  `(setf (gethash ,name *generic-functions*) ,new))

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
        unless (member arg lambda-list-keywords)
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
    (symbol (values name t))
    (list (values (first name) (second name)))))

(defun generate-type-declaration (vars types)
  (assert (= (length vars) (length types)))
  (loop for var in vars
        for type in types
        collect `(type ,type ,var)))

(defun generate-function-name (name types)
  "doesn't dispatch on return type, return type may depend on types of arguments"
  (let* ((types (mapcar (lambda (type)
                          (typecase type
                            (template-variable (binding-of type))
                            (otherwise type)))
                        types)))
    (apply #'symbolicate (interleave (mapcar #'write-to-string (cons name types)) '-))))

;;;;
(defun get-function-by-types (name types)
  (when (member types (gethash name *instantiated-functions*) :test 'equalp)
    (symbol-function (generate-function-name name types))))

(defun instantiate-function-by-types (template types)
  (assert (= (length (template-vars-of template))
             (length types)))
  (let* ((name (name-of template))
         (code (let ((template (copy-template template)))
                 (loop for type in types
                       for var in (template-vars-of template)
                       do (bind-template-variable var type))
                 (instantiate-template template))))
    (when *verbose-p*
      (format t "~&Instantiating generic function: ~s with types: ~s~%" name types))
    (when *debug-p*
      (format t "~&Code:~%~A~%" code)) 
    (eval code)
    (if (null (gethash name *instantiated-functions*))
        (setf (gethash name *instantiated-functions*) (list types))
        (pushnew types (gethash name *instantiated-functions*) :test 'equalp))))

(defmacro defun/t-helper (&key inline-p)
  `(multiple-value-bind (name return-type)
       (extract-name-and-return-type name)
     (multiple-value-bind (types typed-vars)
         (extract-types-and-vars args) 
       (let* ((docstring (when (stringp (first body)) (list (first body))))
              (body (if docstring (rest body) body))
              (lambda-list (extract-ordinary-lambda-list args types))
              (template-vars (remove-duplicates types))
              (template (eval `(deftemplate ,name ,template-vars
                                 progn
                                 (declaim (ftype (function * ,return-type)
                                                 (static-eval (generate-function-name ',name ',template-vars)))
                                          ,,(when inline-p
                                              ``(inline (static-eval (generate-function-name ',name ',template-vars)))))
                                 (defun (static-eval (generate-function-name ',name ',template-vars)) ,lambda-list
                                   ,@docstring
                                   (declare ,@(generate-type-declaration typed-vars types))
                                   ,@body)))))
         (setf (find-generic-function name) template)
         ;; re-instantiate if function redefined
         (loop for types in (gethash name *instantiated-functions*)
               do (instantiate-function-by-types template types))
         ;;
         (with-unique-names (env whole)
           `(defmacro ,name ,(append (list '&whole whole) lambda-list (list '&environment env))
              ,@docstring
              ,(multiple-value-bind (required optional rest keyword allow aux)
                   (parse-ordinary-lambda-list lambda-list)
                 (declare (ignorable allow))
                 (let ((optional (mapcar #'first optional))
                       (keyword (mapcar #'second (mapcar #'first keyword)))
                       (aux (mapcar #'first aux))
                       (rest (when rest (list rest))))
                   `(declare (ignorable ,@(append required optional rest keyword aux)))))
              (declare (optimize (speed 1) safety debug))
              ;;
              (let* ((name ',name)
                     (template (find-generic-function name))
                     (typed-vars ',typed-vars)
                     (typed-var-types
                       (loop for var in typed-vars
                             for type = (variable-type-from-env var ,env)
                             unless type
                             do (simple-style-warning "Cannot get type information of var ~a" var)
                             collect (or type t)))
                     (parameterized-types ',types)
                     (template-vars ',template-vars)
                     (template-var-types
                       (let (collected-template-vars collected-types)
                         (assert (= (length typed-var-types)
                                    (length typed-vars)
                                    (length parameterized-types)))
                         (loop for type in parameterized-types
                               for var-type in typed-var-types
                               for pos = (position type collected-template-vars)
                               if pos
                               do (assert (equalp var-type (nth pos collected-types)))
                               else
                               do (progn
                                    (push type collected-template-vars)
                                    (push var-type collected-types)))
                         (assert (equal (reverse collected-template-vars) template-vars))
                         (nreverse collected-types)))
                     (function-name (generate-function-name name template-var-types))
                     (args (rest ,whole))) 
                (let ((instantiated-types (gethash name *instantiated-functions*)))
                  (unless (and instantiated-types
                               (member template-var-types instantiated-types :test 'equalp)) 
                    (instantiate-function-by-types template template-var-types)))
                ;;
                `(,function-name ,@args))))))))

(defmacro defun/t (name args &body body)
  (defun/t-helper :inline-p nil))

(defmacro defun/t-inline (name args &body body)
  (defun/t-helper :inline-p t))

