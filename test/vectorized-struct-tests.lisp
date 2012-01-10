(in-package :cl-rewrite.test)

(deftest test-parse-defstruct-options ()
  (is (equalp (multiple-value-list (cl-rewrite::parse-defstruct-options 'foo))
              (list 'foo (cl-rewrite::make-defstruct-options))))
  (is (equalp (multiple-value-list (cl-rewrite::parse-defstruct-options '(foo (:conc-name foo-) (:type vector))))
              (list 'foo
                    (cl-rewrite::make-defstruct-options
                     :conc-name 'foo-
                     :type 'vector)))))

(deftest test-parse-defstruct-slot-definition ()
  (is (equalp (cl-rewrite::parse-defstruct-slot-definition 'x)
              (cl-rewrite::make-defstruct-slot-definition :NAME 'X)))
  (is (equalp (cl-rewrite::parse-defstruct-slot-definition '(x 0 :type fixnum))
              (cl-rewrite::make-defstruct-slot-definition
               :NAME 'X
               :INITFORM 0
               :TYPE 'FIXNUM))))

(defstruct foo
  (x 0 :type fixnum)
  (y 0d0 :type double-float))

(defstruct/v foo/v
  (x 0 :type fixnum)
  (y 0d0 :type double-float))

(deftest benchmark-defstruct/v ()
  (declare (optimize speed (safety 0)))
  (is (equal
       (progn
         (sb-ext:gc :full t)
         (format t "~&Defstruct takes...~%")
         (time
          (let ((foos (loop for i of-type array-index below 1000000
                            for j of-type double-float from 0d0
                            collect (make-foo :x i :y j))))
            (list
             (loop for i of-type array-index below 1000000
                   for foo in foos
                   sum (foo-x foo))
             (loop for i of-type array-index below 1000000
                   for foo in foos
                   with sum of-type double-float = 0d0
                   do (incf sum (foo-y foo))
                   finally (return sum))))))
       (progn
         (sb-ext:gc :full t)
         (format t "~&Defstruct/v takes...~%")
         (time
          (let ((foos/v (loop for i of-type array-index below 1000000
                              for j of-type double-float from 0d0
                              collect (make-foo/v :x i :y j))))
            (list
             (loop for i of-type array-index below 1000000
                   for foo/v in foos/v
                   sum (foo/v-x foo/v))
             (loop for i of-type array-index below 1000000
                   for foo/v in foos/v
                   with sum of-type double-float = 0d0
                   do (incf sum (foo/v-y foo/v))
                   finally (return sum)))))))))

