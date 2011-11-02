; arl.lsp                Hiow-Tong See             2 August 1988
; Abbreviated Rule Language for Tmycin

(defun briefrule (rulename)
  (prog (rule premise conclusion)
    (or (setq rule (get rulename 'rule)) (return nil))
    (setq premise (second rule))
    (setq conclusion (third rule))
    (format t "~%Premise :~%")
    (arlpremise premise 0 2 0)
    (format t "~%Action  :~%")
    (if (eq (car conclusion) 'do-all) 
      (mapc #'arlconclusion (cdr conclusion))
      (arlconclusion conclusion))))

(defun arlpremise (clause numb sp initspaces)
  (prog (junction)
    (cond ((null clause) (return nil))
      ((not (member (car clause) '($and $or)))
       (arlclause clause) (return nil)))
    (setq junction (car clause))
 lp (unless (setq clause (cdr clause)) (return nil))
    (spaces (- sp initspaces))
    (setq initspaces 0)
    (setq numb (1+ numb))
    (if (< numb 10) (format t " "))
    (format t "~A) " numb)
    (arlpremise (car clause) 0 (+ sp 5) (+ sp 4))
    (when (cdr clause)(format t "~A~%" 
	    (case junction ($and ",") ($or " or") (t junction))))
    (go lp)))

(defun arlclause (clause)
  (let ((pred (car clause)) 
	(ynparm (eq (fourth clause) 'yes)) symbol sign)
    (setq sign (if (member pred '(notsame thoughtnot notknown)) " ~" " "))
    (cond ((member pred '(same notsame thoughtnot))
	   (if ynparm (format t "~A~A" sign (clauseparm clause))
	     (format t " ~A~A= ~A" (clauseparm clause) sign (fourth clause))))
	  ((setq symbol 
	     (second (assoc pred '((greaterp* ">") (greateq* ">=")
				   (lessp* "<") (lesseq* "<="))))) 
           (format t " ~A ~A ~A" (clauseparm clause) symbol (third clause)))
	  ((eq pred 'between*)
	   (format t " ~A <= ~A <= ~A" (third clause) 
	     (clauseparm clause) (fourth clause)))
	  ((member pred '(known notknown))
	   (format t " ~A is~Aknown" (clauseparm clause) sign)
	   (when (cdddr clause) (format t " to be ~A" (fourth clause)))))))
	
(defun arlconclusion (conc)
  (prog (tally sign)
    (unless (eq (car conc) 'conclude) 
      (return (format t "   Execute ~A~%" conc)))
    (setq tally (sixth conc))
    (setq sign (if (< tally 0) " ~" " "))
    (if (eq (fourth conc) 'yes)
      (format t "  ~A~A" sign (third conc))
      (format t "   ~A~A= ~A" (third conc) sign (fourth conc)))
    (format t " (~D)~%" (/ (float tally) 1000))))

(defun listbriefrules ()
  (mapc #'(lambda (rule) (format t "~%~A" rule) 
	    (briefrule rule)) allrules))


















