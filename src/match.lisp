(in-package :cl-rewrite)

(declaim (inline varp))
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
                (and (eq inp (cdr binding)) bindings) 
                (cons (cons pat inp) bindings)))
          (and (eq pat inp) bindings))))

(defun match (pat inp)
  (%match (list->hlist pat)
          (list->hlist inp)
          '((t . t))))


;;; TODO:
;;; - Add segment matching and binding

