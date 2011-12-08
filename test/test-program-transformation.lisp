(in-package :cl-rewrite.test)

(in-suite cl-rewrite-test)

(deftest test-match-1 ()
  (is (match '(1 (2) 3) '(1 (2) 3)))
  (is (not (match '(1 (2) 3) '(0 (2) 3))))
  (is (not (match '(1 (2) 3) '(1 (0) 3))))
  (is (not (match '(1 (2) 3) '(1 (2) 0))))
  (is (not (match '(1 (2) 3) '(1 2 3))))
  (is (not (match '(1 (2) 3) '((1) (2) 3))))
  (is (not (match '(1 (2) 3) '(1 ((2)) 3)))))

(deftest test-match-2 ()
  (is (eql 1 (cdr (assoc '?x (match '(?x (2) 3) '(1 (2) 3))))))
  (is (eql 2 (cdr (assoc '?x (match '(1 (?x) 3) '(1 (2) 3))))))
  (is (eql 3 (cdr (assoc '?x (match '(1 (2) ?x) '(1 (2) 3))))))
  (is (eql 1 (cdr (assoc '?x (match '(?x (2) ?x) '(1 (2) 1))))))
  (is (not (match '(?x (2) ?x) '(1 (2) 3))))
  (is (not (match '(?x (?x) 3) '(1 (2) 3))))
  (let ((bindings (match '(- (+ ?x ?y) (+ ?z ?y))
                    '(- (+ (age tom) (age mary))
                      (+ (age bill) (age mary))))))
    (is (eql 1 (cdr (assoc '?x bindings))))
    (is (eql 2 (cdr (assoc '?y bindings))))
    (is (eql 3 (cdr (assoc '?z bindings))))))

(deftest test-transf-1 ()
  (is (equal '(- (AGE TOM) (AGE BILL))
             (transf-1 '((- (+ ?x ?y) (+ ?z ?y))
                         (- ?x ?z))
                       '(- (+ (age tom) (age mary))
                         (+ (age bill) (age mary)))))))

(deftest test-transf ()
  (is (equal '(- (AGE TOM) (AGE BILL))
             (transf '(((- (+ ?x ?y) (+ ?z ?y))
                        (- ?x ?z)))
                     '(- (+ (age tom) (age mary))
                       (+ (age bill) (age mary)))))))

(deftest test-transf-all ()
  (is (equal '((- (AGE TOM) (AGE BILL)))
             (transf-all '(((- (+ ?x ?y) (+ ?z ?y))
                            (- ?x ?z)))
                         '(- (+ (age tom) (age mary))
                           (+ (age bill) (age mary)))))))

