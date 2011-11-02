;  gcaux.lsp     Gordon Novak & Bob Causey   15 August 88; 08 Aug 91
;  (formerly gclisp.aux)
;  Auxiliary functions for Golden Common Lisp

; Copyright (c) 1988 by Gordon S. Novak Jr. and Robert L. Causey
; This program may be freely copied, used, or modified,
; provided that this copyright notice is included in each
; copy of the code or parts thereof.

; These functions are designed to allow our exercise programs
; to run under Golden Common Lisp Ver. 1.1, Small Memory.
; They do not always implement the full functionality of the
; corresponding Common Lisp functions.

(unless (and (string-equal (lisp-implementation-type)
                           "GOLDEN COMMON LISP")
             (string-equal (lisp-implementation-version)
                           "1.1, Small Memory"))
         (print
 "Gcaux.lsp is for use in Gold Hill Lisp, Ver 1.1, Small Memory"
            ) )

; Approximate Common Lisp gentemp
(defun gentemp (&optional prefix)
  (intern (symbol-name (gensym (or prefix "T")))) )

; Intersection of sets
(defun intersection (x y)
  (mapcan #'(lambda (item) (if (member item y) (list item)))
	  x) )

; Union of sets.  Uses :test #'equal.
(defun union (x y)
  (prog ()
lp  (cond ((null x) (return y))
          ((member (car x) y :test #'equal))
	  (t (setq y (cons (car x) y))))
    (setq x (cdr x))
    (go lp)))

; Partial implementation of elt, for lists.
(defun elt (seq n)
  (if (and (integerp n) (>= n 0))
      (progn (dotimes (i n) (setq seq (cdr seq))) (car seq))
      (error "Bad index to elt ~A" n)) )

; Partial implementation of the position function, for lists.
; Uses :test #'equal
(defun position (item seq)
  (do ( (idx 0 (1+ idx))
        (lst seq (cdr lst)) )
    ( (or (null lst) (equal item (car lst)) )
      (if lst idx))))

; Bubble sort: not efficient, but okay for small data sets.
(defun sort (seq pred)
  (prog (ptr done tmp)
top (setq done t)
    (setq ptr seq)
lp  (cond ((null (cdr ptr)) (if done (return seq) (go top)))
          ((funcall pred (cadr ptr) (car ptr))
	    (setq done nil)
	    (setq tmp (car ptr))
	    (rplaca ptr (cadr ptr))
	    (rplaca (cdr ptr) tmp)))
    (setq ptr (cdr ptr))
    (go lp) ))

; Error message for defsetf.
(defmacro defsetf (access-fn update-fn &rest rest)
`(format t "~%Sorry, there is no defsetf.~%"))

(defun cadddr (x) (cadr (cddr x)))

; NOTE: In order to run TMYCIN under Golden Common Lisp, it is also
; necessary to edit the functions printconclusion and whynotexpr to
; change the formats ~4,2F to be ~A .  GC Lisp does not support ~F.
