(in-package :cl-rewrite)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +not-implemented-error-string+
      "The feature is not yet implemented for this lisp system"
    :test 'equalp))

(defmacro 1st (&rest args)
  (assert (> (length args) 0))
  (first args))

(defmacro defglobal (name value &optional doc)
  (1st #+sbcl `(sb-ext:defglobal ,name ,value ,doc)
       `(defvar ,name ,value ,doc)))

(defmacro ctime-error (&rest args)
  (apply #'error args))

(defmacro with-not-implemented-error (&body body)
  `(1st ,@body
        (ctime-error #.+not-implemented-error-string+)))

