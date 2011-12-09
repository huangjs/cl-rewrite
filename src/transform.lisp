(in-package :cl-rewrite)

(defun matched-p (transf-result)
  (not (eq transf-result 'match-failure)))

(defun transf-1 (rule input)
  "Transform input according to the rewrite rule.
A rewrite rule is: (input-pattern output-pattern test variable-substitutions)
e.g. ((- (+ ?x ?y) (+ ?z ?y)) (- ?x ?z))
e.g. ((intersection
           (subset (function (lambda (?x) ?p))
                   ?s)
           (subset (function (lambda (?y) ?q))
                   ?s))
          (subset (function (lambda (?x)
                    (and ?p ?qq)))
                  ?s)
          t
          ((?qq (subst ?x ?y ?q))))"
  (let ((bindings (match (first rule) input))
        (test (third rule)))
    (if (and bindings
             (or (null test)
                 (eval (sublisq bindings test))))
        (progn
          (dolist (var (fourth rule))
            (push (cons (car var)
                        (eval (sublisq bindings (cadr var))))
                  bindings))
          (sublis bindings (second rule)))
        'match-failure)))

(defun transf (rules input)
  "Transform input according to the rewrite rules.
A list of rewrite rules: ((input-pattern output-pattern test variable-substitutions) ...)
e.g. (((- (+ ?x ?y) (+ ?z ?y)) (- ?x ?z)) ...)
e.g. (((intersection
           (subset (function (lambda (?x) ?p))
                   ?s)
           (subset (function (lambda (?y) ?q))
                   ?s))
          (subset (function (lambda (?x)
                    (and ?p ?qq)))
                  ?s)
          t
          ((?qq (subst ?x ?y ?q)))))
Note: Only the first rule matched will be applied, so the result is one ouptut"
  (dolist (rule rules)
    (let ((result (transf-1 rule input)))
      (when (matched-p result)
        (return result)))))

(defun transf-all (rules input)
  "Transform input according to the rewrite rules.
A list of rewrite rules: ((input-pattern output-pattern test variable-substitutions) ...)
e.g. (((- (+ ?x ?y) (+ ?z ?y)) (- ?x ?z)) ...)
e.g. (((intersection
           (subset (function (lambda (?x) ?p))
                   ?s)
           (subset (function (lambda (?y) ?q))
                   ?s))
          (subset (function (lambda (?x)
                    (and ?p ?qq)))
                  ?s)
          t
          ((?qq (subst ?x ?y ?q)))))
Note: All rules matched will be applied, so the result is a list of outputs"
  (let (results)
    (dolist (rule rules)
      (let ((result (transf-1 rule input)))
        (when (matched-p result)
          (push result results))))
    (nreverse results)))


;;; TODO:
;;; - Add segment substitution in rewriting rules

