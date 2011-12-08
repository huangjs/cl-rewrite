(in-package :cl-rewrite)

(defun transf-1 (rule input)
  "Transform input according to the rewrite rule.
A rewrite rule is: (input-pattern output-pattern)
e.g. ((- (+ ?x ?y) (+ ?z ?y)) (- ?x ?z))"
  (let ((bindings (match (first rule) input)))
    (when bindings
      (values (sublis bindings (second rule)) t))))

(defun transf (rules input)
  "Transform input according to the rewrite rules.
A list of rewrite rules: ((input-pattern output-pattern) ...)
e.g. (((- (+ ?x ?y) (+ ?z ?y)) (- ?x ?z)) ...)
Note: Only the first rule matched will be applied, so the result is one ouptut"
  (dolist (rule rules)
    (multiple-value-bind (output matched-p)
        (transf-1 rule input)
      (when matched-p
        (return output)))))

(defun transf-all (rules input)
  "Transform input according to the rewrite rules.
A list of rewrite rules: ((input-pattern output-pattern) ...)
e.g. (((- (+ ?x ?y) (+ ?z ?y)) (- ?x ?z)) ...)
Note: All rules matched will be applied, so the result is a list of outputs"
  (let (result)
    (dolist (rule rules)
      (multiple-value-bind (output matched-p)
          (transf-1 rule input)
        (when matched-p
          (push output result))))
    (nreverse result)))

