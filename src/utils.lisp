(in-package :cl-rewrite)

(defun interleave (list thing)
  (loop for l on list
        collect (first l)
        when (rest l)
        collect thing))

(defun variable-type-from-env (var env)
  (with-not-implemented-error
    #+allegro (second (find 'cl:type (nth-value 2 (sys:variable-information var env)) :key #'first))
    #+sbcl (cdr (assoc 'cl:type (nth-value 2 (sb-cltl2:variable-information var env))))))

(defun function-inline-p-from-env (name env)
  (with-not-implemented-error
    #+allegro (eq 'cl:inline (second (assoc 'cl:inline (nth-value 2 (sys:function-information name env t)))))
    #+sbcl (eq 'cl:inline (cdr (assoc 'cl:inline (nth-value 2 (sb-cltl2:function-information name env)))))))

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

(defmacro walk-nodes ((tree &key copy-tree node-var place-var) &body body)
  (once-only (tree)
    (let ((node-var (or node-var (gensym "NODE-VAR")))
          (place-var (or place-var (gensym "PLACE-VAR"))))
      `(labels ((s (subtree place)
                  (cond ((atom subtree)
                         (let ((,node-var subtree)
                               (,place-var place))
                           (declare (ignorable ,node-var ,place-var))
                           (locally ,@body)))
                        (t
                         ,(when copy-tree
                            `(let* ((car (s (car subtree) subtree))
                                    (cdr-tree (cdr subtree))
                                    (cdr (when cdr-tree (s cdr-tree subtree)))) 
                               (cons car cdr)))))))
         (s ,tree ,tree)))))


(defmacro walk-subtree ((tree &key copy-tree subtree-var) &body body)
  (once-only (tree)
    (let ((subtree-var (or subtree-var (gensym "SUBTREE-VAR"))))
      `(labels ((s (subtree)
                  (let* ((,subtree-var subtree)
                         (result (locally ,@body)))
                    (declare (ignorable ,subtree-var result))
                    (if (eq result subtree)
                        (loop for e in subtree
                                 ,(if copy-tree 'collect 'do)
                                 (if (listp e)
                                     (s e)
                                     e))
                        result))))
         (s ,tree)))))


;;;;
;; Generic (shallow) object copying for CLOS
;; Author: Michael Weber <michaelw@foldr.org>, 2008

;;; Notes:
;; This is a simple, but rather slow method of copying objects.  It
;; would be faster if COPY-INSTANCE methods would be partially
;; specialized on the class.  In other words: unroll the slot-copying
;; loop, and optoinally get rid of SLOT-BOUNDP checks if the users
;; says it is safe.  Care would need to be taken to regenerate this
;; specialized method if the result of COPY-CLASS-SLOTS changes,
;; either by class redefinition, or by adding methods on
;; COPY-CLASS-SLOTS.

;;;; Code
(defgeneric copy-class-slots (class)
  (:documentation "Returns the set of slots of CLASS which are
considered for copying by COPY-INSTANCE.

If CLASS is of type STANDARD-CLASS, all slots \(as returned by
CLASS-SLOTS) are considered.")
  (:method ((class standard-class))
    (with-not-implemented-error
        #+sbcl (sb-pcl:class-slots class)
        #+allegro (mop:class-slots class))))

(defgeneric make-uninitialized-instance (class)
  (:documentation "Allocates a fresh uninitialized instance of the
given class CLASS.

If CLASS is of type CLASS, ALLOCATE-INSTANCE is used.")
  (:method ((class class))
    (allocate-instance class)))

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a \(shallow) copy of OBJECT.

An uninitialized object of the same class as OBJECT is allocated by
calling MAKE-UNINITIALIZED-INSTANCE.  For all slots returned by
\(COPY-CLASS-SLOTS \(CLASS-OF OBJECT)), the returned object has the
same slot values and slot-unbound status as OBJECT.

REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (make-uninitialized-instance class)))
      (dolist (slot (copy-class-slots class))
        (let ((slot-name
                (with-not-implemented-error
                    #+sbcl (sb-pcl:slot-definition-name slot)
                    #+allegro (mop:slot-definition-name slot))))
          (when (slot-boundp object slot-name)
            (setf (slot-value copy slot-name)
                  (slot-value object slot-name)))))
      (apply #'reinitialize-instance copy initargs))))

;;; sublis, with quoting value of bindings
(defun kwote (x)
  (if (constantp x)
      x
      (list 'quote x)))

(defun sublisq (bindings form)
  (sublis
   (mapcar #'(lambda (x)
               (cons (car x) (kwote (cdr x))))
           bindings)
   form))

