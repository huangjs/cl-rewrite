(in-package :cl-rewrite)

(defun varp (v)
  "Test for pattern variables, e.g. ?x"
  (and (symbolp v)
       (char= #\? (char (symbol-name v) 0))))

(defun %match (pat inp bindings)
  "Do real pattern matching."
  (if (consp pat)
      (and (consp inp)
           (%match (cdr pat)
                   (cdr inp)
                   (%match (car pat)
                           (car inp)
                           bindings)))
      (if (varp pat)
          (let ((binding (assoc pat bindings)))
            (if binding
                (and (equal inp (cdr binding)) bindings) 
                (cons (cons pat inp) bindings)))
          (and (equal pat inp) bindings))))

(defun match (pat inp)
  (%match pat inp '((t . t))))


;;; TODO:
;;; - Add segment matching and binding

