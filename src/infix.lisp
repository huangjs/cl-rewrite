(in-package :cl-rewrite)

(defparameter *operators* '(+ - * / ^ %)) 

(defun infix->prefix (exp)
  "Transform infix expression to prefix expression.
Note:
- only operators have infix syntax.
- function application syntax is fun(arg1 arg2 ...).
- can do higher order functions.
- operator priority is ignored, use () for ordering!
- right associative!"
  (cond ((atom exp)
         exp)
        ;; (f) => f
        ((null (cdr exp))
         (infix->prefix (car exp)))
        ;; (f . t) => (f), it's a hack
        ((eq (cdr exp) t)
         `(,(infix->prefix (car exp))))
        (t
         (destructuring-bind (1st 2nd &rest rest) exp 
           (cond ((listp 2nd) ; could be () 
                  `(,(infix->prefix 1st)
                    ,(or (infix->prefix 2nd) t)
                    ,@rest))
                 ((and (member (second exp) *operators*))
                  `(,2nd
                    ,(infix->prefix 1st)
                    ,(infix->prefix rest)))
                 (t
                  exp))))))

