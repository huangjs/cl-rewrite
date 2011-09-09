(in-package :cl-rewrite.test)

(defsuite* (cl-rewrite-test :in root-suite :documentation "cl-rewrite tests"))

(in-suite cl-rewrite-test)

(defvar *v1*)
(defvar *v2*)
(defvar *t1*)
(defvar *t2*)
(defvar *template*)

(defixture empty-template-variables
  (:setup (setf *v1* (make-instance 'cl-rewrite::template-variable :name 'v1)
                *v2* (make-instance 'cl-rewrite::template-variable :name 'v2)))
  (:teardown (setf *v1* nil *v2* nil)))

(deftest test-copy-template-variable (&optional source copied)
  (with-fixture empty-template-variables
    (let* ((source (or source *v1*))
           (v1 (or copied (cl-rewrite::copy-template-variable source))))
      (is (not (eq v1 source)))
      (is (eq (cl-rewrite::name-of v1) (cl-rewrite::name-of source)))
      (is (eq (cl-rewrite::binding-of v1) (cl-rewrite::binding-of source)))
      (setf (cl-rewrite::binding-of v1) 'new)
      (is (not (eq (cl-rewrite::binding-of v1) (cl-rewrite::binding-of source))))
      (let ((v2 (cl-rewrite::copy-template-variable v1)))
        (is (not (eq v1 v2)))
        (is (eq (cl-rewrite::name-of v1) (cl-rewrite::name-of v2)))
        (is (eq (cl-rewrite::binding-of v1) (cl-rewrite::binding-of v2)))
        (setf (cl-rewrite::binding-of v2) 'old)
        (is (not (eq (cl-rewrite::binding-of v1) (cl-rewrite::binding-of v2))))))))

(deftest test-template-variable-matches-p (&optional v)
  (with-fixture empty-template-variables
    (let ((v (or v *v1*)))
      (is (eq t (cl-rewrite::template-variable-matches-p v 'anything)))
      ;; write more tests for variables of other classes
      )))

(deftest test-bind-template-variable (&optional v)
  (with-fixture empty-template-variables
    (let ((v (or v *v1*)))
      (cl-rewrite::bind-template-variable v 'new)
      (is (eq 'new (cl-rewrite::binding-of v))))
    ))

(deftest test-template-variable-boundp (&optional v)
  (with-fixture empty-template-variables
    (let ((v (or v *v1*)))
      (is (not (cl-rewrite::template-variable-boundp v)))
      (cl-rewrite::bind-template-variable v 'new)
      (is (cl-rewrite::template-variable-boundp v)))))

(deftest test-instantiate-template-variable (&optional v)
  (with-fixture empty-template-variables
    (let ((v (or v *v1*)))
      (cl-rewrite::bind-template-variable v 'new)
      (is (eq 'new (cl-rewrite::instantiate-template-variable v))))))


;;;
(defixture default-template
  (:setup
   (with-fixture empty-template-variables
     (setf *template*
           (make-instance 'cl-rewrite::template
                          :name 'default-template
                          :template-vars (list *v1* *v2*)
                          :body `(+ (the ,*v1* x) (the ,*v2* y))))))
  (:teardown
   (setf *template* nil)))

(deftest test-copy-template (&optional source copied)
  (with-fixture default-template
    (let* ((source (or source *template*))
           (copied (or copied (cl-rewrite::copy-template source))))
      (is (not (eq source copied)))
      (is (eq (cl-rewrite::name-of copied) (cl-rewrite::name-of source)))
      (is (not (eq (cl-rewrite::template-vars-of copied) (cl-rewrite::template-vars-of source))))
      (is (not (eq (cl-rewrite::body-of copied) (cl-rewrite::body-of source)))) 
      (cl-rewrite::bind-template-variable (cl-rewrite::find-template-variable 'v1 copied) 'fixnum)
      (cl-rewrite::bind-template-variable (cl-rewrite::find-template-variable 'v2 copied) 'double-float) 
      (let ((copied (cl-rewrite::copy-template copied)))
        (is (not (eq source copied)))
        (is (eq (cl-rewrite::name-of copied) (cl-rewrite::name-of source)))
        (is (not (eq (cl-rewrite::template-vars-of copied) (cl-rewrite::template-vars-of source))))
        (is (not (eq (cl-rewrite::body-of copied) (cl-rewrite::body-of source)))) 
        ))))

(deftest test-instantiate-template (&optional template)
  (with-fixture default-template
    (let ((template (or template *template*)))
      (cl-rewrite::bind-template-variable (cl-rewrite::find-template-variable 'v1 template) 'fixnum)
      (cl-rewrite::bind-template-variable (cl-rewrite::find-template-variable 'v2 template) 'double-float) 
      (is (equal (cl-rewrite::instantiate-template template)
                 '(+ (the fixnum x) (the double-float y)))))))

(deftest test-deftemplate ()
  (let ((template (eval '(deftemplate foo (t1 t2)
                          (+ (the t1 x) (the t2 y))))))
    (cl-rewrite::bind-template-variable (cl-rewrite::find-template-variable 't1 template) 'fixnum)
    (cl-rewrite::bind-template-variable (cl-rewrite::find-template-variable 't2 template) 'double-float) 
    (is (equal (cl-rewrite::instantiate-template template)
               '((+ (the fixnum x) (the double-float y)))))))

