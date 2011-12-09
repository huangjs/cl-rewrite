(in-package :cl-rewrite)

;; Structure sharing are important technique for reducing equality
;; tests in symbolic computation. Hash-consing is a well-known
;; technique

(defparameter *hash-consing-table*
  (with-not-implemented-error
    #+sbcl (make-hash-table :test 'equal :weakness :value)
    #+allegro (make-hash-table :test 'equal :values :weak)
    #+lispworks (make-hash-table :test 'equal :weak-kind :value)
    #+ccl (make-hash-table :test 'equal :weak :value)
    #+cmucl (make-hash-table :test 'equal :weak-p :value)))

(defparameter *hcons-memoize-table*
  (with-not-implemented-error
    #+sbcl (make-hash-table :test 'eq :weakness :value)
    #+allegro (make-hash-table :test 'eq :values :weak)
    #+lispworks (make-hash-table :test 'eq :weak-kind :value)
    #+ccl (make-hash-table :test 'eq :weak :value)
    #+cmucl (make-hash-table :test 'eq :weak-p :value)))

(defun hcons (x y)
  "Hash-consing"
  (gethash-or-set (cons x y) *hash-consing-table* (cons x y)))

(defun memoize-hcons (cons hcons)
  (setf (gethash cons *hcons-memoize-table*) hcons))

(defun list->hlist (lst)
  "Turn a list to a hash-consed list, for maximum structure sharing.
Also memoize all the sublists, this will speed up transformation if
same inputs are used repeatedly.

Note: it might share structure with previous hlists, so be careful
  about the destructive list operations!"
  (when (gethash lst *hcons-memoize-table*)
    (return-from list->hlist lst))
  (if (atom lst)
      lst
      (memoize-hcons
       lst
       (hcons (list->hlist (car lst))
              (list->hlist (cdr lst))))))
