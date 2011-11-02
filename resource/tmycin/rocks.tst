;  rocks.tst         Gordon Novak       06 June 88

; Copyright (c) 1988 by Gordon S. Novak Jr.
; This program may be freely copied, used, or modified,
; provided that this copyright notice is included in each
; copy of the code or parts thereof.

(load "rocks.lsp")

; The following are examples to examine the knowledge base:
(showrule 'rule101)     ; Print out a rule in Lisp form
(englrule 'rule101)     ; Print out a rule in English form
allrules                ; Look at the list of all rules

; The following is an example consultation, including answers
; to be provided by the user in response to questions:
(doconsult 'rock)
  (black 0.7)
  why
  5.7
  ?
  ((metamorphic 0.2)(igneous 0.8))

; The following show variations on "WHY" questions regarding the
; previous consultation.  Note that the symbol ROCK66 should be
; replaced by whatever symbol was generated for the last
; consultation.
(why)
(why obsidian)
(why identity obsidian)
(why rock66 identity obsidian)
(whynot coal)
(whynot identity coal)
(whynot rock66 identity coal)

; The following will print out the internal storage of
; properties for the preceding consultation.
; Note: The proper symbol should be substituted for ROCK66.
(showprops 'rock66)

; A second consultation example:
(doconsult 'rock)
  black
  3
  no
  sedimentary

; A third consultation example:
(doconsult)
  white
  sedimentary
(why)
