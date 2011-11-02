@Device(Imagen300)
@Make(Article)
@Define(PE,FaceCode T)
@Heading(TMYCIN Expert System Tool)
@Set(Page=1)
@Blankspace(.3 inch)
@Center{Gordon S. Novak Jr.
Computer Science Department
University of Texas at Austin
Austin, TX  78712}
@Blankspace(.3 inch)

@Section(Introduction)

TMYCIN (``Tiny EMYCIN'') is a simple expert system tool patterned after
the EMYCIN tool developed at Stanford @Cite[MYCIN]@Cite[Emycin].
TMYCIN does not attempt to provide all of the features of EMYCIN; it is
intended to provide some of the most commonly used features in a package
that is small and simple.  The internal implementation of TMYCIN has been
written from scratch and is therefore different from that of EMYCIN.

@Section(Data)

Data about a particular case is stored in a record structure called a
@I[context]; the current context is referenced in rules by the global
variable @PE[cntxt].  TMYCIN allows only a single level of context.

@Subsection(Context Description)

The data in a context are described in a @I[context description] or
@I[class], which is defined to the system by a call to @PE[DEFCONTEXT]:
@Example{

(defcontext <context-name>
	    <parameters>
	    <initial-data>
	    <goals>)
}
where @PE[<context-name>] is the name of the context (usually the name of
the kind of object being identified or diagnosed, e.g., @PE[ROCK] or @PE[PATIENT]),
@PE[<parameters>] is a list of parameter descriptions, @PE[<initial-data>]
is a list of the parameters whose values are to be asked for
at the start of every consultation, and @PE[<goals>] is a list of parameters
whose values are sought as the result of the consultation.  An example of a
call to @PE[DEFCONTEXT] is shown below.
@Begin(Example)


(defcontext 'rock                           ; Context Name
            '((color (brown black white))   ; Parameters
	      (hardness posnumb)
	      (environment (igneous metamorphic sedimentary)
                 ("What is the type of geologic environment"
                  "in which the specimen was found?"))
	      (identity atom)
	      (pretty nil))                  ; Yes/No parm
	    '(color)                         ; Initial data
	    '(identity))                     ; Goals


@End(Example)

Each @PE[<parameter>] description is a list of three items:
@Example{

(<parameter-name> <type> <prompt>)

}
@Begin(Enumerate)
The @PE[<parameter-name>] is always a single symbol, e.g. @PE[COLOR].

The @PE[<type>] of the parameter may be:
@Enumerate{
A type as used in EMYCIN, e.g. @PE[POSNUMB] (positive number).  This is
not checked, but is shown to the user when prompting for user input.

A list of possible values, e.g. @PE[(BROWN BLACK WHITE)].  This is
not checked, but is shown to the user when prompting for user input.

@PE[NIL], which indicates a Yes/No parameter.  This value is examined by
the input routine, which will convert an input of @PE[NO] to @PE[(YES@ -1.0)]
if the type is @PE[NIL].  A type of @PE[NIL] will also affect the way in
which the parameter is described when prompting for input.
}

The @PE[<prompt>] (optional) may be either a string or a list of strings.
The prompt string(s) are shown to the user when prompting for input, or in
response to a ``?'' input; the purpose is to explain in more detail the
input that is being asked for (including, for example, units of measurement).
@End(Enumerate)

@Subsection(Internal Data Storage)

A context in TMYCIN is implemented as a Lisp symbol (a @PE[GENSYM] symbol formed
from the context name, e.g. @PE[ROCK37]) with the data values
stored on its property list.  A pointer to the context description is stored
under the property name @PE[ISA].

Each parameter value is stored as a list of values and certainty factors, e.g.
@Example{

COLOR ((RED 0.4)(WHITE 0.3)(BLUE 0.1))

}
where @PE[COLOR] is the parameter/property name and the values are stored as
a list under that property.  The values are sorted by certainty factor (CF),
with the value having the highest CF first.

@Section(Rules)
@Subsection(Rule Format)
Rules are defined to the system using the function @PE[DEFRULES], which takes
one or more unquoted rules as input.  Each rule has the format:
@Example{

(<rulename> <premises>
            <conclusion>)

}
Usually the @PE[<premises>] is a conjunction of conditions grouped within
a call to the function @PE[$AND] , and the conclusion is a call to the
function @PE[CONCLUDE] (the function @PE[DO-ALL] can be used for multiple
conclusions or for actions in addition to calling @PE[CONCLUDE]).  An example
of a rule definition is shown below.
@Example{

(defrules 
 
(rule101 ($and (same cntxt color black)
	       (notsame cntxt pretty yes)
	       ($or (between* (val1 cntxt hardness) 3 5)
		    (same cntxt environment sedimentary)))
	 (conclude cntxt identity coal tally 400))

)

}
@Subsection($AND and $OR)

The condition part of a rule is usually formed from tests of parameter values,
combined by the functions @PE[$AND] and @PE[$OR].  @PE[$AND] evaluates each
of its clauses in order.  If any clause returns @PE[NIL] or returns a CF
(@I[certainty factor]) value less than the .2 threshold, @PE[$AND] immediately
returns @PE[NIL] (without evaluating any other clauses); otherwise, @PE[$AND]
returns the minimum CF that any clause returned.  @PE[$OR] is ``true'' if any
of its clauses is ``true''; @PE[$OR] returns the maximum CF returned by any of
its clauses.  @PE[$AND] and @PE[$OR] may be nested.

Before trying all of its clauses, @PE[$AND] does a ``prescan'' of the clauses
to see if any of them is already known to be false (CF of @PE[NIL] or below
the .2 threshold); if so, @PE[$AND] returns @PE[NIL] without evaluating any
of the other clauses.

@Subsection(Testing Data Values)

Several functions are provided to test data values within rules.  These differ
in terms of the certainty factor required to make the condition ``true'' and
in terms of the CF value returned.  Each of these functions uses the global
variable @PE[CNTXT], which is a pointer to the current context.
@Itemize{
@PE[(SAME CNTXT <parameter> <value>)] tests whether the specified parameter
has the specified value with CF@ >@ .2 ; the value returned is the CF
of the parameter.  A Yes/No parameter is tested for the value ``Yes'' with
@PE[(SAME@ CNTXT@ <parameter>@ YES)].

@PE[(NOTSAME CNTXT <parameter> <value>)] tests whether the specified parameter
does not have the specified value at all, or has it with CF@ <=@ .2 ; if so, the
value returned is 1.0 .  Note that @PE[NOTSAME] returns ``true'' for a parameter
whose value is ``unknown''.

@PE[(THOUGHTNOT CNTXT <parameter> <value>)] tests whether the specified parameter
has the specified value with CF@ <@ -.2 ; if so, the value returned is the negative
of the CF value.  @PE[THOUGHTNOT] is the negative counterpart of @PE[SAME].
A Yes/No parameter is tested for the value ``No'' with
@PE[(THOUGHTNOT@ CNTXT@ <parameter>@ YES)].  @PE[THOUGHTNOT] requires a negative
value for the specific parameter; it will not respond to an ``unknown'' value.

@PE[(KNOWN CNTXT <parameter>)] tests whether the specified parameter
has a value with CF@ >@ .2 (or, for yes/no parameters, CF@ <@ -.2); if so, the
value returned is always 1.0, which means that the CF of data tested using
@PE[KNOWN] will not affect the CF returned by @PE[$AND].

@PE[(NOTKNOWN CNTXT <parameter>)] tests whether the specified parameter
has no value with CF@ >@ .2 (or, for yes/no parameters, CF@ <@ -.2); if so, the
value returned is 1.0 .  This predicate may be used to test for data that the
user specifies as ``unknown''.
}

@Subsection(Numeric Tests)

In order to perform numeric tests or other calculations with parameter values,
it is first necessary to get the numeric values of the parameters; this is done
using the function @PE[VAL1].  @PE[VAL1] gets the value of a parameter which
has the highest CF of all the possible values stored for that parameter.
The format is:
@Example{

(VAL1 CNTXT <parameter>)

}
If the parameter has a numeric value, @PE[VAL1] will return that value, which
can then be used in numeric calculations or in the numeric comparison functions
described below.

The numeric comparison functions provided with TMYCIN are different from the
plain Lisp comparison functions in two respects: they are able to tolerate
non-numeric arguments (in which case they return @PE[NIL]), and they return
a value of 1.0 if the test is ``true'' so that they can be used within
@PE[$AND].  The comparison functions are:
@Example{

(greaterp* <numexp1> <numexp2>)
(greateq*  <numexp1> <numexp2>)
(lessp*    <numexp1> <numexp2>)
(lesseq*   <numexp1> <numexp2>)
(between*  <numexp1> <numexp2> <numexp3>)

}
@PE[BETWEEN*] tests whether @PE[<numexp2> <= <numexp1> < <numexp3>].

@Section(Input)

When TMYCIN asks for a parameter value during a consultation, there are several
kinds of response the user can give:
@Enumerate{
The user can simply enter a single data value, e.g. @PE[BLUE].  The resulting
stored value will be a list of that one value with a certainty of 1.0, i.e.,
@PE[((BLUE 1.0))].  For a Yes/No parameter, the user may enter @PE[YES], @PE[Y],
@PE[NO], or @PE[N]; @PE[NO] and @PE[N] are converted to @PE[((YES -1.0))] .

The user can enter a list of a single value and a certainty factor, e.g.,
@PE[(YES@ 0.6)].  The resulting stored value will be a list of that one value,
i.e., @PE[((YES@ 0.6))].

The user can enter a list of multiple values and certainty factors, e.g.,
@PE[((RED@ 0.5)(ORANGE@ 0.5))].  TMYCIN does not enforce any kind of
consistency among certainty factors.  Therefore, a parameter that is thought
of as multivalued may have several parameters with high certainty, e.g.,
@PE[((RED@ 1.0)@ (WHITE@ 1.0)@ (BLUE@ 1.0))]

The user can enter @PE[UNK] or @PE[UNKNOWN] if the value of the parameter
is unknown.  This results in a ``dummy'' data set, @PE[((UNKNOWN@ 0.0))] .
In general, unknown values will not be considered as ``true'' by the
predicates that test data values (with the exception of @PE[NOTSAME] and
@PE[NOTKNOWN]), so that most rules involving unknown data will not fire.

The user can enter @PE[?].  The system will respond by printing the
prompt string for the parameter (if there is one) and the type specified
for the parameter.  Then the user will be asked for the parameter value again.
Note that with @PE[*printdes*] set to @PE[T], the default value, this
information is printed automatically; the @PE[?] input is mainly useful
when @PE[*printdes*] is set to @PE[NIL].

The user can enter @PE[WHY].  The system will respond by printing the
rule that is currently being examined, then ask for the parameter value again.
}

@Subsection(Input Options)

The global variable @PE[*printdes*] determines whether the data type and prompt
information will be shown to the user automatically when asking for input.
The default value of @PE[*printdes*] is @PE[T].

If desired, the user may define a function to obtain input, either from the user
or from another source (e.g., a database or special I/O device).  If the parameter
name has the property @PE[ASKFN] defined, the function specified for the @PE[ASKFN]
property will be called to obtain the value; its parameters are the data context and
the parameter name.  The @PE[ASKFN] should return a list of @PE[(<value>@ <cf>)]
pairs, as described above.  For example, if an @PE[ASKFN] reads a @PE[VOLTAGE] of 4.6
volts from an A/D converter, it should return @PE[((4.6@ 1.0))] .
@Example{

; Specify the `ask' function for voltage
(setf (get 'voltage 'askfn) #'readvoltage)

; Simulate reading of voltage: 4 volts + 0-1 volts noise
(defun readvoltage (cntxt parm)
  (list (list (+ 4.0 (random 1.0)) 1.0)) )

}

In some cases, it may be desirable to ask the user for a value before trying to
infer a parameter value using rules.  For example, the @PE[TEMPERATURE] of a patient
will usually be known.  If the @PE[ASKFIRST] property is defined with a non-@PE[NIL]
value for a parameter name, then the user will be asked for a value before rules
to infer the parameter are tried.
@Example{

(setf (get 'temperature 'askfirst) t)

}

@Section(Escaping to Lisp)

An important feature of an expert system tool is the ability to escape from it
into the underlying programming language to perform computations that are not
easily supported by the tool.  TMYCIN provides several places at which an expert
system application can escape into Lisp.

@Subsection(Special Input Functions)

As mentioned above, it is possible to put a function name on the property list
of a parameter name as the value of the @PE[ASKFN] property.  This will cause that
function to be called for input of that parameter rather than asking the user for
the value.  This allows some or all of the input data used by the expert system to
be gotten from other sources, such as databases, data files, or special input
devices.  If desired, the parameters could also be specified as @PE[ASKFIRST]
parameters so that data will be obtained for them before the consultation begins.

@Subsection(Functions in Rule Premises)

A call to an arbitrary function can be included in the premise of a rule.
Such a function may use the global variable @PE[CNTXT] to refer to the current
data context.  The function should return a value which is a certainty factor
(a number from -1.0 to 1.0), or @PE[NIL] to indicate failure.  Consult the
existing predicate functions for examples.  If no value is known for a parameter
and the global variable @PE[*prescan*] is non-@PE[NIL], the value @PE[1.0]
should be returned.

@Subsection(Calculations in Rule Conclusions)

If the @PE[<value>] part of a @PE[CONCLUDE] call is a single atom (symbol), it is
treated as if it were quoted:
@Example{

(conclude cntxt identity coal tally 400)
}
In this case, @PE[coal] is treated as a quoted value.  However, if the @PE[<value>]
is a list, it is evaluated; this allows calculations to be performed in the
conclusion part of a rule.  The function @PE[VAL1] can be used to get parameter
values.
@Example{

(rule107 ($and (same cntxt shape circle)
	       (known cntxt radius))
	 (conclude cntxt area 
		   (* 3.14159
		      (expt (val1 cntxt radius) 2))
		   tally 1000))
}

@Subsection(Calculations in Rule Certainty Factors)

In some cases, it is desirable to use data values in calculating certainty factors.
For example, suppose that a physician expert specifies that lung cancer is ruled
out for patients less than 20 years old, and is to be linearly weighted negatively
for patients from 20 to 30 years old.
@Example{

(rule112 (lesseq* (val1 cntxt age) 30)
	 (conclude cntxt diagnosis lung-cancer
		   tally (if (< (val1 cntxt age) 20)
			     -1000
			     (* (- 30 (val1 cntxt age))
				-100))) )
}

@Subsection(Calling Functions From Conclusion)

It is sometimes useful to call user functions in the conclusion part of a rule.
The function @PE[DO-ALL] is used to perform multiple actions in the conclusion
part of a rule.  By putting both a @PE[CONCLUDE] call and a call to a user
function within the @PE[DO-ALL], the user function can be called when the rule
fires.  At least one @PE[CONCLUDE] call is needed in order to cause the rule
to be considered during backchaining; the parameter that is @PE[CONCLUDE]d
should either be a goal parameter or one that will be traced in seeking the
value of a goal parameter.
@Example{

(rule109 ($and (same cntxt smoke yes)
	       (same cntxt heat yes))
	 (do-all (conclude cntxt problem fire tally 800)
		 (sound-fire-alarm-fn)))
}

@Section(Notes on Using TMYCIN)

@Subsection(Deleting Rules)

In most cases, when a rule is edited and defined again using @PE[DEFRULES],
no further action is necessary.  However, if the rule conclusion is changed
so that it no longer concludes the same parameter, or if the rule is to be
deleted entirely, special action is necessary.  The reason for this is that
rules are indexed so that each parameter has a list of the rules that conclude
it; if a rule is to be deleted, it must be removed from this list.

@PE[(delrule@ <rulename>)]@Foot[These functions were written by Hiow-Tong Jason See.]
will delete the single rule @PE[<rulename>].

@PE[(clear-rules)] will delete all rules.

@Subsection(Self-Referencing Rules)

Sometimes rules are written that reference the same parameter in both the
premise and conclusion; these might be used, for example, to increase the
certainty of a parameter that is already concluded if additional factors
are present.  In order to run such rules correctly, all other rules
that conclude the parameter must run first.  To make this happen, all
self-referencing rules should be put at the @I[end] of the file of rules;
this will make them run last in the ordering used by TMYCIN.
@Example{

(rule119 ($and (same cntxt identity rattlesnake)
	       (same cntxt bit-someone yes)
	       (same cntxt victim-died yes))
	 (conclude cntxt identity rattlesnake tally 800))
}

@Section(Useful Functions)

@Subsection(Starting the Consultation)

@PE[DOCONSULT] is used to start a new consultation; its optional parameter is a
context class name.
@Example{

(doconsult)

(doconsult 'rock)

}

@Subsection(Why Questions)

Two functions, @PE[WHY] and @PE[WHYNOT], are provided to allow the user to
ask about the system's reasoning at the end of a consultation.
@PE[WHY] will print out the rule(s) that concluded values for a parameter.
@PE[WHYNOT] asks why a particular conclusion was @I[not] reached.  It prints
rule(s) that might have reached the specified conclusion along with the
failing condition that prevented each rule from firing.

Both functions allow omission of parameters for convenience.  The full set of
parameters is:
@Example{

(why    <context> <parameter> <value>)

(whynot <context> <parameter> <value>)

}
It is permissible to omit the first parameter, the first two, or all three
(in the case of the @PE[WHY] function).
The @PE[<context>] defaults to the context for the most recently run
consultation.  The @PE[<parameter>] defaults to the first parameter in the
@PE[GOALS] list.  The @PE[<value>] defaults to the most strongly concluded
value for the @PE[<parameter>].  Example calls are:
@Example{

(why)
(why obsidian)
(why identity obsidian)
(why rock37 identity obsidian)

(whynot coal)
(whynot identity coal)
(whynot rock37 identity coal)

}

@Subsection(Translation of Rules into English)

The function @PE[ENGLRULE] translates a previously defined rule into an
English-like format.  Its argument is a quoted rule name:
@Example{

(englrule 'rule101)

}
produces:
@Example{

If:
      1) the COLOR of the ROCK is BLACK, and
      2) PRETTY is not true of the ROCK, and
      3)   1) the HARDNESS of the ROCK is 4, or
           2) the ENVIRONMENT of the ROCK is SEDIMENTARY
then:
      there is weakly suggestive evidence (0.4)
      that the IDENTITY of the ROCK is COAL
}

@Subsection(Analysis of the Knowledge Base)

@PE[SHOWRULE] will pretty-print a rule in internal (Lisp) form.
@Example{

(showrule 'rule101)

}

@PE[LISTPARMS]@Foot[These functions were written by Hiow-Tong Jason See.]
will print out the parameters defined for the current context
class (gotten from the current value of @PE[CNTXTNAME] , which will be set to
the context name used in the last call to @PE[DEFCONTEXT]).
							  
@PE[LISTRULES] will list all the rules, in both English and internal forms.

@PE[ANALYZE-KB] will analyze the knowledge base, listing the rules that can
conclude each parameter.

@PE[LISTKB] will print a complete listing of the knowledge base, i.e., it does
@PE[ANALYZE-KB], @PE[LISTPARMS], and @PE[LISTRULES].

@Subsection(Other Useful Functions)

@PE[SHOWPROPS] will pretty-print the property list of an atom, which is useful
for looking at the data values stored for a particular consultation:
@Example{

(showprops 'rock37)

}

@Section(How TMYCIN Works)

The basic operation of TMYCIN is very simple.  The function @PE[doconsult],
which is the top-level function that performs a consultation, first makes
a new symbol from the context name (e.g., @PE[ROCK37]) to be the context
for the consultation.  Next, it asks the user for values of the parameters
that are specified as initial-data parameters in the context definition.
Then it calls @PE[bc-goal] to find values for each of the goal parameters.
Finally, it prints the results.

The basic function to find values for a parameter (called @I[tracing] the
parameter) is @PE[bc-goal], so named because it @I[backchains] on rules to
try to find values for its goal parameter.  @PE[bc-goal] finds the set of
rules that could conclude some value for the desired parameter and runs
each of them; if there are no rules to conclude the value of a parameter,
it asks the user for the value.  The rules that can conclude each parameter
are stored on the property list of the parameter under the property
@PE[RULES]; this cross-indexing is done by @PE[DEFRULES] when the rules
are defined.  For example, @PE[RULE101], which concludes that the identity
of a rock is coal, is stored as one of the rules under the property
@PE[RULES] of the symbol @PE[IDENTITY].  Note that while @PE[bc-goal] will
often be called by a predicate seeking a particular value for a parameter,
e.g. @PE[(SAME@ CNTXT@ COLOR@ WHITE)], it will try to run all rules that
conclude @I[any] value for the parameter (in this case, @PE[COLOR]) before
it quits.

Running a rule is done in two stages.  First, the antecedent (left-hand side
or ``if'' part) of the rule is evaluated; if it has a ``true'' value, i.e., a CF
greater than 0.2, the consequent (``then'' part) of the rule is executed.
Evaluating the antecedent will typically involve tests of
parameter values using predicates such as @PE[SAME]; these predicates call
the function @PE[parmget] to get the value(s) of the parameter.  @PE[parmget]
returns an existing value if there is one; otherwise, it calls @PE[bc-goal]
to find the value.  In this way, @PE[bc-goal] does a depth-first search of
the rule tree, where the root(s) of the tree are goal parameters and the
terminal nodes of the tree are parameter values provided by the user as
input.

@Section(References)
@Bibliography

@Newpage
@Appendix(Listing of TMYCIN Code)

@Set(page=26)
@Newpage
@Appendix(TMYCIN Test File: Rocks)
