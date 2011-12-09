(in-package :cl-rewrite)

;; Structure sharing are important technique for reducing equality
;; tests in symbolic computation. Hash-consing is a well-known
;; technique

(defvar *hash-consing-hashes*
  (with-not-implemented-error
    #+sbcl (make-hash-table :test 'equal :weakness :value)
    #+allegro (make-hash-table :test 'equal :values :weak)
    #+lispworks (make-hash-table :test 'equal :weak-kind :value)
    #+ccl (make-hash-table :test 'equal :weak :value)
    #+cmucl (make-hash-table :test 'equal :weak-p :value)))

(defun hcons (x y)
  "Hash-consing"
  (gethash-or-set (cons x y) *hash-consing-hashes* (cons x y)))

(defun list->hlist (lst)
  "Turn a list to a hash-consed list, for maximum structure sharing.
Note: it might share structure with previous hlists, so be careful
  about the destructive list operations!"
  (if (atom lst)
      lst
      (hcons (list->hlist (car lst))
             (list->hlist (cdr lst)))))

