(in-package :cl-rewrite)

(defun interleave (list thing)
  (loop for l on list
        collect (first l)
        when (rest l)
        collect thing))

(defun variable-type-from-env (var env)
  nil
  #+sbcl (cdr (assoc 'cl:type (nth-value 2 (sb-cltl2:variable-information var env)))))

(defmacro gethash-or-set (key table gen-value)
  (alexandria:with-unique-names (val present)
    (alexandria:once-only (key table)
      `(multiple-value-bind (,val ,present)
           (gethash ,key ,table)
         (if ,present
             ,val
             (setf (gethash ,key ,table) ,gen-value))))))
