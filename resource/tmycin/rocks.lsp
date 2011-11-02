;  rocks.lsp          Gordon Novak       06 June 88; 11 Feb 96
;  TMYCIN Example: very small expert system to identify rocks

; Copyright (c) 1988 by Gordon S. Novak Jr.
; This program may be freely copied, used, or modified,
; provided that this copyright notice is included in each
; copy of the code or parts thereof.

; Note: Do not load both snakes.lsp and rocks.lsp at the same time.

; (load "/u/cs381k/tmycin/tmycin.lsp")             ; Load the system

(defcontext 'rock                              ; Top context
            '((color (brown black white))      ; Parameters
	      (hardness posnumb)
	      (environment (igneous metamorphic sedimentary)
                "What is the type of geologic environment?")
	      (identity atom)
	      (pretty nil             ; use nil for yes/no parms
		"What an average person would consider pretty"))
	    '(color)                  ; Initialdata parameters
	    '(identity))              ; Goals

(defrules 
(rule101 ($and (same cntxt color black)
	       (notsame cntxt pretty yes)
	       ($or (same cntxt hardness 4)
		    (same cntxt environment sedimentary)))
	 (conclude cntxt identity coal tally 400))

(rule102 ($and (same cntxt color black)
	       (between* (val1 cntxt hardness) 5 7)
	       (notsame cntxt environment sedimentary))
         (conclude cntxt identity obsidian tally 700))

(rule103 ($and (same cntxt color white)
	       (same cntxt environment sedimentary))
	 (do-all (print "looks like limestone to me")
		 (conclude cntxt identity limestone tally 800)))
)
