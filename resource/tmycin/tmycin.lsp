; tmycin.lsp         Common Lisp version           ; 29 Nov 06
;
; TMYCIN = Tiny EMYCIN-like Expert System Tool
;
; Copyright (c) 2006  Gordon S. Novak Jr. and The University of Texas at Austin
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;
; This program, originally developed for use in a course for
; Hewlett Packard, simulates the backchaining expert system tool
; EMYCIN.  Obviously, given the size of this system, many
; features of EMYCIN are not supported; however, it is suitable
; for student exercises.  The program is written from scratch;
; the internals are different from those of EMYCIN.
;
; Input of parameter values may be in several forms:
;      RED                     one value with CF = 1.0
;      (RED 0.6)               one value with specified CF
;      ((RED 0.3)(BLUE 0.3))   multiple values and CF's
;      ?                       Ask for explanation.
;
; Internal Representation:
;
; Context: a Gensym atom with an ISA pointer to the Class atom,
;          PARENT pointer to higher-level context.
;          (Note: only a single level of context is implemented.)
; 
; Data values are stored on property list under the parameter
; name as as a list of (Value CF) pairs.
;
; Additions:  Improved Printconclusion (Jeff Nowell, HP).
;             Fix macro-expansion problem in Gold Hill (Bob Causey).
;             Auxiliary functions (Hiow-Tong Jason See).
;(in-package :user)

(defvar allrules nil)         ;  List of all rules, for user.
(defvar cntxt nil)            ;  Current data context
(defvar topctx nil)           ;  Top context of consultation
(defvar cntxtname nil)        ;  Name of the top context class
(defvar tally 0)              ;  For CF calculations
(defvar prevgoals nil)        ;  Loop prevention in bc-goal
(defvar runrulerulename nil)  ;  Used in answering why question
(defvar *printdes* t)         ;  To always print parm prompt
(defvar *prescan* nil)        ;  To preview rule antecedent

(setq cntxt nil)
(setq tally nil)
(setq allrules nil)

; Copy and then eval.  This crock is included for Gold Hill Lisp,
; which bashes macro calls.  For better Lisps, eval can be used.
(defun tmycin-eval (x) (eval (copy-tree x)))

;----------  Functions for defining the Knowledge Base ----------

; Define a context class.
; Args are: context name, parameters, initialdata, goals.  Each
; parameter spec is a list of parameter name, expected type, and
; prompt string.  NIL is the expected type for YES/NO parameters.
(defun defcontext (contextname parms initialdata goals)
    (setq cntxtname contextname)
    (setf (get contextname 'parameters) parms)
    (setf (get contextname 'initialdata) initialdata)
    (setf (get contextname 'goals) goals)
    contextname)

; Define a set of rules
(defmacro defrules (&rest lvarfordefrules)
  `(progn (mapc #'defrule (quote ,lvarfordefrules)) t))

; Define a new rule, index on conclusions, add to ALLRULES.  ; 05 Jan 90
; A rule looks like: (RULE101 ($AND ...) (CONCLUDE ...))
(defun defrule (rule)
  (let (parms concpart rulename)
    (setq rulename (if (eq (car rule) 'if) (make-atom 'rule)
		       (car rule)))
    (setf (get rulename 'rule) rule)
    (checkpre rulename (cadr rule))
    (setq concpart (caddr rule))
    (setq parms (cond ((and (eq (car concpart) 'conclude)
			    (eql (length concpart) 6))
	                (list (caddr concpart)))
		      ((and (eq (car concpart) 'do-all)
			    (mapcan #'(lambda (conc)
					(if (and (eq (car conc) 'conclude)
						 (eql (length conc) 6))
					    (list (caddr conc)) ))
				    (cdr concpart)) ))
		      (t (format t "Bad CONCLUDE part of rule ~A~%"
				rulename))) )
    (mapc #'(lambda (parm)
              (unless (member rulename (get parm 'rules))
		 (setf (get parm 'rules)
		       (nconc (get parm 'rules)
			      (list rulename)))))
          parms)
    (unless (member rulename allrules)
      (setq allrules (nconc allrules (list rulename)))) ))

; Check preconditions of rules for correct number of args  ; 05 Jan 90
(defun checkpre (rulename clause)
  (let ((fn (first clause)) pair)
    (if (member fn '($and $or)) (mapc #'(lambda (x) (checkpre rulename x))
				      (rest clause))
        (if (setq pair
		  (assoc fn '((same . 3) (notsame . 3) (thoughtnot . 3)
			      (known . 2) (notknown . 2) (lessp* . 2)
			      (greaterp* . 2) (greateq* . 2)
			      (lesseq* . 2) (between* . 3))))
	    (unless (eql (cdr pair) (length (rest clause)))
		    (format t "Wrong number of args to ~A in rule ~A~%"
			      fn rulename))))))

;----------  Functions to run a Consultation  -------------------

; Run a consultation.  E.g., (DOCONSULT 'ROCK)
(defun doconsult (&optional topclass)
  (let (goals)
    (unless topclass (setq topclass cntxtname))
    (terpri) (terpri)
    (setq goals (get topclass 'goals))
    (setq topctx (make-context topclass nil))
    (getinitialdata topctx)
    (setq *prescan* nil)
    (mapc #'(lambda (goal) (bc-goal topctx goal (list goal)))
	  goals)
    (format t "~%~%The conclusions for ~A are as follows:~%" 
              topctx)
    (mapc #'(lambda (goal) (printconclusion topctx goal)) goals)
    topctx ))

; Make a new data context of CLASS with a pointer to PARENT
; context.  Example:  CLASS = CULTURE and PARENT = PATIENT1
(defun make-context (class parent)
  (let ((cntxt (make-atom class)))
    (setf (get cntxt 'isa) class)
    (if parent (setf (get cntxt 'parent) parent))
    cntxt ))

; Make a gensym atomname with a specified name.
(defun make-atom (name)
  (let (newatom n)
    (setq n (or (get name 'gensymnumber) 0))
    (setf (get name 'gensymnumber) (1+ n))
    (setq newatom
	  (intern (concatenate 'string (symbol-name name)
                               (princ-to-string n))))
; If this atom has something on its proplist, try again. 
    (if (or (get newatom 'rule) (get newatom 'isa))
        (make-atom name)
        newatom) ))

; Get initial data for a class
(defun getinitialdata (cntxt)
  (let ((class (get cntxt 'isa)))
    (mapc #'(lambda (parm)
              (storeparmvals cntxt parm (askvalue cntxt parm)))
	  (get class 'initialdata)) ))

; Backchain through rules to try to conclude goal
(defun bc-goal (cntxt parm prevgoals)
  (prog ((cntxtp (findparmctx cntxt parm)) rules asked)
; If parm has already been traced, don't do it again.
    (cond ((get cntxtp parm))
          (*prescan* (return nil)))
    (setq rules (get parm 'rules))
    (if (setq asked (or (null rules) (get parm 'askfirst)))   
        (storeparmvals cntxtp parm (askvalue cntxtp parm)))
    (unless (hasvalue cntxtp parm)
	    (mapc #'(lambda (rule) (runrule cntxt rule)) rules))
    (cond ((hasvalue cntxtp parm))
          ((not asked)
	   (storeparmvals cntxtp parm (askvalue cntxtp parm)))
	  (t (setf (get cntxtp parm)
		   (list (list 'unknown 0.0)))))
    (return (get cntxtp parm)) ))

; Find proper level of context for a parameter
(defun findparmctx (ctxt parm)
  (let ((class (get ctxt 'isa)))
    (if (and ctxt class) 
        (if (assoc parm (get class 'parameters))
	    ctxt
	    (findparmctx (get ctxt 'parent) parm))
	(if ctxt (error "Context has no Class")
	   (error "Parm ~A misspelled or left out of defcontext"
		  parm))) ))

; Ask for a parameter value
(defun askvalue (cntxt parm)
  (let ((askfn (get parm 'askfn)))
    (if askfn (funcall askfn cntxt parm) (askuser cntxt parm)) ))

; Ask the user for a data value
(defun askuser (cntxt parm)
  (prog (inp parmdes)
    (setq parmdes (findparmdes cntxt parm))
top (askparmquestion cntxt parm parmdes)
    (if *printdes* (progn (terpri) (printparmdes parmdes)))
lp  (terpri)
    (setq inp (read))
    (cond ((eq inp '?) (printparmdes parmdes) (go lp))
	  ((eq inp 'why) (terpri)
	    (princ "We are examining the following rule:")
	    (terpri) (terpri) (showrule runrulerulename)
	    (terpri) (go top))
          ((member inp '(y yes n no))
	    (if (null (cadr parmdes))      ; test for yes/no parm
		(setq inp (list 'yes (if (member inp '(n no))
				         -1.0 1.0)))))
	  ((member inp '(unk unknown))
	    (setq inp (list 'unknown 0.0))) )
    (return (if (consp inp)
	        (if (consp (car inp)) inp (list inp))
		(list (list inp 1.0)) )) ))

; Find description for a parameter
(defun findparmdes (ctxt parm)
  (let (class)
    (if (and ctxt (setq class (get ctxt 'isa)))
        (or (assoc parm (get class 'parameters))
	    (findparmdes (get ctxt 'parent) parm))
	(if ctxt (error "Context has no Class")
	   (error "Parm ~A misspelled or left out of defcontext"
		  parm))) ))

; Print description of a parameter
(defun printparmdes (parmdes)
  (if (cddr parmdes)
    (mapc #'(lambda (x) (spaces 4) (princ x) (terpri))
	  (if (consp (caddr parmdes)) (caddr parmdes)
	      (list (caddr parmdes)))))
  (format t "    Expected values are: ~A~%"
    (or (cadr parmdes) "Yes/No")) )

; Ask the question associated with parameter parm.
(defun askparmquestion (cntxt parm parmdes)
  (format t (if (cadr parmdes) "What is the ~A of ~A?"
	                       "Is ~A true of ~A?")
    parm cntxt) )

; Store values for a parameter, sorting by CF.
(defun storeparmvals (ctxt parm vals)
  (setf (get ctxt parm)
	(sort vals #'(lambda (u v) (> (cadr u) (cadr v))) ) ) )

; Test if a value is defined for a parameter
(defun hasvalue (ctxt parm)
  (let ((vals (get ctxt parm)))
    (and vals (not (eq (caar vals) 'unknown))) ))

; Run a rule: if antecedent is true, execute consequent.
(defun runrule (cntxt rulename)
  (let ((rule (get rulename 'rule))(runrulerulename rulename)
	tally)
    (setq tally (tmycin-eval (cadr rule)))
    (if (and tally (numberp tally) (> tally 0.2))
	(tmycin-eval (caddr rule))) ))

;----------  Functions for Left-Hand Side of a Rule  ------------

; AND together clauses so long as CF > .2; return minimum CF.
(defmacro $and (&rest conditions)
  `($andexpr (quote ,conditions)))
(defun $andexpr (clauses)
  (let (pre)
    (setq *prescan* t)
    (setq pre ($andexprb clauses 1.0))
    (setq *prescan* nil)
    (if pre ($andexprb clauses 1.0)) ))
(defun $andexprb (clauses cf)
  (let (cr)
    (if clauses (if (setq cr (eval (car clauses)))
		    (if (and (numberp cr) (> cr 0.2))
			($andexprb (cdr clauses) (min cf cr))))
                cf) ))

; OR together clauses, return maximum CF, stop on CF = 1.0.
(defmacro $or (&rest conditions)
  `($orexpr (quote ,conditions) nil))
(defun $orexpr (clauses cf)
  (let (cr)
    (if clauses
        (if (setq cr (eval (car clauses)))
	    (setq cf (if cf (if (numberp cr) (max cf cr) cf)
		            cr))))
    (if (and (numberp cf) (= cf 1.0)) cf
        (if (cdr clauses) ($orexpr (cdr clauses) cf) cf)) ))

; Get a parameter value, backchaining for it if needed.
(defun parmget (cntxt parm)
  (or (cntxtget cntxt parm)
      (unless (member parm prevgoals)          ; to prevent loops
	      (bc-goal cntxt parm (cons parm prevgoals)))) )

; Get value of parameter from a context, looking up parent chain
(defun cntxtget (cntxt parm)
   (if cntxt (or (get cntxt parm)
		 (cntxtget (get cntxt 'parent) parm))) )

; Test for a specified value with CF > .2, return CF.
(defmacro same (cntxt parm value)
  `(sameexpr ,cntxt (quote ,parm) (quote ,value)))
(defun sameexpr (cntxt parm value)  
  (let ((vals (parmget cntxt parm)) pair)
    (if (and (setq pair (assoc value vals)) (> (cadr pair) 0.2))
        (cadr pair)
	(if (and (null vals) *prescan*) 1.0)) ))

; Test for a specified value with CF <= .2, return 1.0
(defmacro notsame (cntxt parm value)
  `(notsameexpr ,cntxt (quote ,parm) (quote ,value)))
(defun notsameexpr (cntxt parm value)  
  (let ((vals (parmget cntxt parm)) pair)
    (if (or (null (setq pair (assoc value vals)))
	    (<= (cadr pair) 0.2))
        1.0) ))

; Test for a specified value with CF < -.2, return -CF.
(defmacro thoughtnot (cntxt parm value)
  `(thoughtnotexpr ,cntxt (quote ,parm) (quote ,value)))
(defun thoughtnotexpr (cntxt parm value)  
  (let ((vals (parmget cntxt parm)) pair)
    (if (and (setq pair (assoc value vals)) (< (cadr pair) -0.2))
        (- (cadr pair))
	(if (and (null vals) *prescan*) 1.0)) ))

; Test for some value with CF > .2, return 1.0
(defmacro known (cntxt parm)
  `(knownexpr ,cntxt (quote ,parm)))
(defun knownexpr (cntxt parm)  
  (let ((vals (parmget cntxt parm))
	(des (findparmdes cntxt parm)))
    (if (some #'(lambda (pair) (or (> (cadr pair) 0.2)
				   (and (null (cadr des))
					(< (cadr pair) -0.2))))
	      vals)
        1.0
	(if (and (null vals) *prescan*) 1.0)) ))

; Test for no value with CF > .2, return 1.0 if there is none.
(defmacro notknown (cntxt parm)
  `(notknownexpr ,cntxt (quote ,parm)))
(defun notknownexpr (cntxt parm)  
  (let ((vals (parmget cntxt parm))
	(des (findparmdes cntxt parm)))
    (unless (some #'(lambda (pair)
		      (or (> (cadr pair) 0.2)
			  (and (null (cadr des))
			       (< (cadr pair) -0.2))))
		  vals)
            1.0) ))

; Get the value of a parameter.
; The value with highest CF is returned.
(defmacro val1 (cntxt parm)
  `(val1expr ,cntxt (quote ,parm)))
(defun val1expr (cntxt parm) (caar (parmget cntxt parm)) )

; EMYCIN-style comparisons
(defun greaterp* (x y)
  (if (and x y (numberp x) (numberp y) (> x y)) 1.0
      (if (and *prescan* (or (null x) (null y))) 1.0) ))

(defun greateq* (x y)
  (if (and x y (numberp x) (numberp y) (>= x y)) 1.0
      (if (and *prescan* (or (null x) (null y))) 1.0) ))

(defun lessp* (x y)
  (if (and x y (numberp x) (numberp y) (< x y)) 1.0
      (if (and *prescan* (or (null x) (null y))) 1.0) ))

(defun lesseq* (x y)
  (if (and x y (numberp x) (numberp y) (<= x y)) 1.0
      (if (and *prescan* (or (null x) (null y))) 1.0) ))

(defun between* (x low high)
  (if (and x low high (numberp x) (numberp low) (numberp high)
	   (<= low x) (< x high))
      1.0
      (if (and *prescan* (or (null x) (null low) (null high)))
	  1.0) ))

;----------  Functions for Right-Hand Side of a Rule  -----------

; Basic rule conclusion function
;  (CONCLUDE CNTXT parm val TALLY cf)
(defmacro conclude (cntxt parm value tally rulecf)
  `(concludeexpr ,cntxt (quote ,parm) (quote ,value)
		 ,tally ,rulecf))
(defun concludeexpr (cntxt parm val tally rulecf)
  (let ((cntxtp (findparmctx cntxt parm)) vals
	pair (newcf (* tally (/ (float rulecf) 1000.0)))
	(vall (if (atom val) val (eval val))))
    (setq vals (get cntxtp parm))
    (if (setq pair (assoc vall vals))
	(rplaca (cdr pair) (cfcombine (cadr pair) newcf))
	(push (list vall newcf) vals))
    (storeparmvals cntxtp parm vals) ))

; EMYCIN CF calculation algorithm.
; "It ain't perfect, but it's better than its inputs usually are."
(defun cfcombine (old new)
  (cond ((null old) new)
        ((null new) old)
        ((not (and (numberp old) (numberp new)))
	  (error "Bad CF combination: ~A ~A" old new))
        ((or (and (= old 1.0) (= new -1.0))
             (and (= old -1.0) (= new 1.0)))
	  (error "Contradiction in CF values: ~A ~A" old new))
	((or (= old 1.0) (= new 1.0)) 1.0)
	((or (= old -1.0) (= new -1.0)) -1.0)
	((and (minusp old) (minusp new))
	  (- (cfcombine (- old) (- new))))
	((and (not (minusp old)) (not (minusp new)))
	  (+ old (* new (- 1.0 old))))
	(t (/ (+ old new) (- 1.0 (min (abs old) (abs new)))))) )

; Print conclusion for a parameter
(defun printconclusion (cntxt parm)
  (terpri) (print parm) (princ ":")
  (mapc #'(lambda (x)
	    (cond ((and (numberp (cadr x)) (> (cadr x) 0.2))
		    (format t "  ~A (~4,2F)" (car x) (cadr x)))
	          ((and (eq (car x) 'yes) (numberp (cadr x))
			(< (cadr x) -0.2))
		    (format t "  ~A (~4,2F)" 'no (- (cadr x))))))
        (cntxtget cntxt parm))
  (terpri) )

; Do a set of things in consequent
(defmacro do-all (&rest lvarfordoall)
   `(mapc (function eval) (quote ,lvarfordoall) ))

;----------  Utility Functions for Printing and Analysis  -------

; Show a rule in Lisp form, e.g., (showrule 'rule101).
(defun showrule (rulename) (pprint (get rulename 'rule)))

; Show properties of a context, e.g., (showprops 'rock3)
(defun showprops (atm) (pprint (cons atm (symbol-plist atm))))

; The following code translates rules to English, answers Why.
; Uses CNTXTNAME, the last context given to defcontext.
; Print out a rule in stylized English
(defun englrule (rulename)
  (prog ((rule (get rulename 'rule)) conse)
    (unless rule (return nil))
    (terpri) (terpri) (princ "If:") (terpri)
    (printpremises (cadr rule) 0 5 0)
    (terpri) (princ "then:") (terpri)
    (setq conse (caddr rule))
    (if (eq (car conse) 'conclude)
        (printconc conse)
        (mapc #'printconc (cdr conse)))
    (terpri)
    (return rulename) ))

;  Print premises of a rule
(defun printpremises (clause numb spaces initspaces)
  (prog (junction)
    (cond ((null clause)(return nil))
	  ((not (member (car clause) '($and $or)))
	   (printclause clause) (return nil)))
    (setq junction (car clause))
 lp (unless (setq clause (cdr clause)) (return nil))
    (spaces (- spaces initspaces))
    (setq initspaces 0)
    (setq numb (1+ numb))
    (if (< numb 10) (princ " "))
    (princ numb) (princ ") ")
    (printpremises (car clause) 0 (+ spaces 5) (+ spaces 4))
    (when (cdr clause) (princ ", ")
	    (princ (case junction ($and "and") ($or "or")
			          (t junction)))
	    (terpri))
    (go lp) ))
	    
(defun spaces (n) (dotimes (i n) (princ " ")) )

(defun printclause (clause)
(let ((pred (car clause)) pair)
  (cond ((member pred '(same notsame thoughtnot))
          (cond ((eq (car (cdddr clause)) 'yes)
		  (princ (clauseparm clause))
		  (princ (if (eq pred 'same) " is " " is not "))
		  (princ "true of the ") (princ cntxtname))
		(t (princ "the ") (princ (clauseparm clause))
		   (princ " of the ") (princ cntxtname)
   		   (princ (if (eq pred 'same) " is " " is not "))
		   (princ (car (cdddr clause))) )))
	((setq pair 
	   (assoc pred '((greaterp* "greater than ")
			 (greateq* "greater than or equal to ")
			 (lessp* "less than")
			 (lesseq* "less than or equal to")
			 (between* "between "))))
	 (princ "the ") (princ (clauseparm clause))
	 (princ " of the ") (princ cntxtname) (princ " is ")
	 (princ (cadr pair)) (princ (caddr clause))
	 (when (eq pred 'between*) (princ " and ")
	       (princ (car (cdddr clause)))))
	((eq pred 'known) (princ "the ")
	 (princ (clauseparm clause)) (princ " of the ")
	 (princ cntxtname) (princ " is known")
	 (when (cdddr clause) (princ " to be ")
		   (princ (car (cdddr clause)))))) ))

;  Return the parameter name for a clause		  
(defun clauseparm (clause)
  (case (car clause)
        ((same notsame known thoughtnot) (caddr clause))
	((greaterp* greateq* lessp* lesseq* between*)
	   (caddr (cadr clause)))
	(t (caddr clause))) )

(defun printconc (conc)
  (prog (tally)
    (unless (eq (car conc) 'conclude) (return nil))
    (setq tally (caddr (cdddr conc)))
    (if (and (numberp tally) (>= (abs tally) 1000))
	(princ "it is definite")
        (progn (princ "      there is")
	       (princ (cond ((not (numberp tally)) " ")
			    ((< (abs tally) 500) " weakly ")
			    ((>= (abs tally) 800) " strongly ")
			    (t " ")))
	       (princ "suggestive evidence")))
    (princ " (") (princ (if (numberp tally)
			    (/ (float tally) 1000.0) tally))
    (princ ")") (terpri) (princ "      that the ")
    (cond ((eq (car (cdddr conc)) 'yes) (princ cntxtname)
	    (princ (if (and (numberp tally) (< tally 0))
		       " is not " " is "))
	    (princ (caddr conc)) )
	  (t (princ (caddr conc)) (princ " of the ")
	     (princ cntxtname)
	     (princ (if (and (numberp tally) (< tally 0))
		        " is not " " is "))
	     (princ (car (cdddr conc))) ))
    (terpri) ))

; (why cntxt parm value) asks why a value was concluded.
; value is optional; if unspecified, it defaults to the value
; concluded most strongly.   Example:  (why rock34 identity)
(defmacro why (&optional cntxt parm value)
  `(whynotexpr (quote ,cntxt) (quote ,parm) (quote ,value) t))

; (whynot cntxt parm value) asks why a value was not concluded.
; Example:  (whynot rock34 identity coal)
(defmacro whynot (cntxt &optional parm value)
  `(whynotexpr (quote ,cntxt) (quote ,parm) (quote ,value) nil))
(defun whynotexpr (cntxt parm value whyflg)
  (prog (rules rulename rule tally concpart vals ante pair topc)
    (if (null value)
        (cond (parm (setq value parm) (setq parm cntxt)
		    (setq cntxt topctx))
	      (t (setq value cntxt) (setq cntxt topctx))))
    (unless (setq topc (get cntxt 'isa))
            (princ cntxt) (princ " ??") (return nil))
    (unless parm (setq parm (car (get topc 'goals))))
    (if (and whyflg (null value))
        (setq value (val1expr cntxt parm)))
    (setq rules (get parm 'rules))
 lp (unless (setq rulename (pop rules)) (return cntxt))
    (setq rule (get rulename 'rule))
; See if this rule concluded the value of interest
    (setq concpart (caddr rule))
    (setq vals (mapcan #'(lambda (conc)
			   (if (and (eq (car conc) 'conclude)
				    (eq (caddr conc) parm))
			     (list (list (car (cdddr conc))
					 (caddr (cdddr conc))))))
		       (if (eq (car concpart) 'conclude)
			   (list concpart)
			   (cdr concpart))))
    (unless (setq pair (assoc value vals)) (go lp))
; Evaluate the precondition and see if it worked.
    (unless whyflg (setq *prescan* t))
    (setq tally (tmycin-eval (cadr rule)))
    (setq *prescan* nil)
    (if (if whyflg (not (and tally (numberp tally)))
                   (and tally (numberp tally) (> tally 0.2)))
        (go lp))
    (terpri) (terpri)
    (princ "The following rule ")
    (unless whyflg (princ "might have "))
    (princ "concluded that") (terpri)
    (princ "the ") (princ parm) (princ " of ") (princ cntxt)
    (princ " was ") (princ value)
    (if whyflg (format t " (~4,2F)"
	         (* tally (/ (float (cadr pair)) 1000.0)) ))
    (terpri) (terpri)
    (showrule rulename)
    (if whyflg (go lp))
    (terpri)
    (princ "but it failed on the following condition:")
    (terpri) (terpri)
    (setq ante (cadr rule))
    (unless (eq (car ante) '$and) (pprint ante) (go lp))
lpb (unless (setq ante (cdr ante)) (go lp))
    (setq *prescan* t)
    (setq tally (tmycin-eval (car ante)))
    (setq *prescan* nil)
    (if (and tally (numberp tally) (> tally 0.2)) (go lpb))
; This clause failed.  Print it and its result.
    (pprint (car ante))
    (princ "returned CF value was ") (princ tally) (terpri)
    (go lp) ))

; The following functions were written by Hiow-Tong Jason See

;   List out all the parmeters defined in the current context.
(defun listparms ()
  (format t 
    "~%~%~%--- PARAMETER LISTING OF ~a KNOWLEDGE BASE --- ~%~%"
    cntxtname)
  (dolist (x (get cntxtname 'parameters))  (format t
      "~a~%  TRANS    : ~a~%  EXPECT   : ~a~%  PROMPT : ~a~%~%"
      (first x) (printtran x) (second x) (third x))))

(defun printtran (parm)
  (if (second parm) (format nil "the ~a of * is" (car parm))
                    (format nil "~a is true of *" (car parm))) )

;  List out all the rules, in a nice format.
(defun listrules ()
  (format t "~%~%~%--- RULE LISTING OF ~a KNOWLEDGE BASE --- ~%"
            cntxtname)
  (dolist (x allrules)
    (format t "~%~%~%~a" x)
    (englrule x)
    (format t "Premise :~%")
    (pprint (cadr (get x 'rule)))
    (format t "Action  :~%")
    (pprint (caddr (get x 'rule))) ))

;   List out parameters, showing rules that might derive them.
(defun analyze-kb ()
  (format t "--- ANALYSIS OF ~a KNOWLEDGE BASE --- ~%" cntxtname)
  (format t "~%Number of Rules in the KB      : ~d" 
    (length allrules))
  (format t "~%Number of Parameters in the KB : ~d" 
    (length (get cntxtname 'parameters)))
  (format t "~%Initial Data needed by the KB  : ~a"
   (get cntxtname 'initialdata)) 
  (format t "~%Goals to be concluded          : ~a"
   (get cntxtname 'goals)) 
  (format t " ~%~%~%Deriviation of Parameters:~%")
  (dolist (x (get cntxtname 'parameters))
    (format t "~%~a~%   " (first x))
    (cond ((get (first x) 'rules) 
	   (format t "Derived from : ") 
	   (pprint (get (first x) 'rules)))
          (t (format t "Need input from user.~% " x)))))

;   List out everything in the Expert Knowledge Base.
(defun listkb ()
  (analyze-kb)
  (listparms)
  (listrules))

;   Clear all previous rules in the knowledge base.
(defun clear-rules ()
  (dolist (p (get cntxtname 'parameters))
	   (setf (get (car p) 'rules) nil))
  (setf allrules nil))

; Delete a single rule
(defun delrule (rulename)
  (dolist (p (get cntxtname 'parameters))
    (if (member rulename (get (car p) 'rules))
        (setf (get (car p) 'rules)
	      (remove rulename (get (car p) 'rules)))))
  (setf allrules (remove rulename allrules))
  rulename)
