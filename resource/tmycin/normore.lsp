; AND together clauses so long as CF > .2; return minimum CF.
(defmacro $and-n-or-more (&rest conditions)
  `($andnormoreexpr (quote ,conditions)))
(defun $andnormoreexpr (clauses)
  (prog (cr nneeded (n 0) (cf 1.0))
    (setq nneeded (pop clauses))
 lp (if clauses (if (setq cr (eval (pop clauses)))
		    (if (and (numberp cr) (> cr 0.2))
			(progn (setq cf (min cf cr))
			       (if (> (incf n) nneeded)
				   (return cf)))))
                (return nil))
    (go lp) ))

