;  snakes.tst          Gordon Novak       08 June 88

;  Tests of expert system to identify snakes of Travis County, TX

; Copyright (c) 1988 by Gordon S. Novak Jr.
; This program may be freely copied, used, or modified,
; provided that this copyright notice is included in each
; copy of the code or parts thereof.

(load "tmycin.lsp")
(load "snakes.lsp")

; For the first example consultation, the prompts given by
; the system are shown as comments.

(doconsult)

; What is the COLOR of SNAKE1183?
;     List the colors of the snake.  If there are
;     multiple colors, use the following format:
;     ((RED 1.0) (YELLOW 1.0) (BLACK 1.0))
;     Expected values are: (TAN BROWN BLACK GREY GREEN
;                           PINK RED YELLOW ORANGE)
((red 1.0) (yellow 1.0) (black 1.0))

; What is the SIZE of SNAKE1183?
;     Give the approximate size of the snake.
;     tiny = 10 inches or less, small = 11-18 inches,
;     medium = 19-30 inches, large = over 30 inches.
;     Expected values are: (TINY SMALL MEDIUM LARGE)
medium

; What is the THICKNESS of SNAKE1183?
;     Compared to other snakes, is this snake
;     quite thin, medium, or heavy-bodied?
;     Expected values are: (THIN MEDIUM FAT)
medium

; What is the PATTERN of SNAKE1183?
;     What pattern(s) are seen on the snake?
;     stripes  = one or more stripes running lengthwise
;     bands    = multiple bands around the body
;     blotches = large contrasting blotches on the back
;     spots    = small, roughly circular spots
;     speckles = non-circular contrasting spots
;     solid    = solid color without pattern
;     If multiple patterns are observed, enter all
;     using the format ((feature 1.0) ...)
;     Expected values are: (BANDS STRIPES BLOTCHES DIAMONDS
;                                 SPOTS SPECKLES SOLID)
bands

; Is RED-AND-YELLOW true of SNAKE1183?
;     Are red and yellow bands adjacent?
;     Expected values are: Yes/No
yes

; The conclusions for SNAKE1183 are as follows:
;
; IDENTITY :  TEXAS-CORAL-SNAKE (0.80)
;
; LATIN-NAME :  Micrurus fulvius tenere (0.80)
;
; POISONOUS :  YES (0.80)
