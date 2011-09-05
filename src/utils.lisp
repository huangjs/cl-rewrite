(in-package :cl-rewrite)

(defparameter *verbose-p* t)
(defparameter *rebind-warning-p* t)
(defparameter *debug-p* t)

(defun interleave (list thing)
  (loop for l on list
        collect (first l)
        when (rest l)
        collect thing))

(defun variable-type-from-env (var env)
  nil
  #+allegro (second (find 'cl:type (nth-value 2 (sys:variable-information var env)) :key #'first))
  #+sbcl (cdr (assoc 'cl:type (nth-value 2 (sb-cltl2:variable-information var env)))))

(defmacro local-variable-type (var &environment env)
  (variable-type-from-env var env))

(defmacro gethash-or-set (key table gen-value)
  (alexandria:with-unique-names (val present)
    (alexandria:once-only (key table)
      `(multiple-value-bind (,val ,present)
           (gethash ,key ,table)
         (if ,present
             ,val
             (setf (gethash ,key ,table) ,gen-value))))))

(defmacro walk-tree ((tree &key copy-tree node-var place-var) &body body)
  (once-only (tree)
    (let ((node-var (or node-var (gensym "NODE-VAR")))
          (place-var (or place-var (gensym "PLACE-VAR"))))
      `(labels ((s (subtree place)
                  (cond ((atom subtree)
                         (when subtree
                           (let ((,node-var subtree)
                                 (,place-var place))
                             (declare (ignorable ,node-var ,place-var))
                             ,@body)))
                        (t (let ((car (s (car subtree) subtree))
                                 (cdr (s (cdr subtree) subtree)))
                             (declare (ignorable car cdr))
                             ,(when copy-tree
                                `(cons car cdr)))))))
         (s ,tree ,tree)))))

