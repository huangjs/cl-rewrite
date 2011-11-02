;  snakes.lsp          Gordon Novak       15 June 88; 11 Feb 96

;  TMYCIN Example: expert system to identify snakes
;                  of Travis County, Texas

; Copyright (c) 1988 by Gordon S. Novak Jr.
; This program may be freely copied, used, or modified,
; provided that this copyright notice is included in each
; copy of the code or parts thereof.

; Ref: Alan Tennant, "A Field Guide to Texas Snakes",
;      Texas Monthly Press, 1985.

; "W. C. Fields liked to tell people he always kept some whiskey
;  handy in case he saw a snake - which he also kept handy."

; (load "/u/cs381k/tmycin/tmycin.lsp")        ; Load the system first

(defcontext 'snake
 '((color (tan brown black grey green pink red yellow orange)
	  ("List the colors of the snake.  If there are"
	   "multiple colors, use the following format:"
	   "((color 1.0) ...), for example,"
	   "((RED 1.0) (YELLOW 1.0) (BLACK 1.0))"))
   (size (tiny small medium large)
	  ("Give the approximate size of the snake."
	   "tiny = 11 inches or less, small = 12-18 inches,"
	   "medium = 19-30 inches, large = over 30 inches."))
   (thickness (thin medium heavy)
	  ("Compared to other snakes, is this snake"
	   "quite thin, medium, or heavy-bodied?"))
   (pattern (bands stripes blotches diamonds spots speckles
		   solid)
	  ("What pattern(s) are seen on the snake?"
	   "stripes  = one or more stripes running lengthwise"
	   "bands    = multiple bands around the body"
	   "blotches = large contrasting blotches on the back"
	   "spots    = small, roughly circular spots"
	   "speckles = non-circular contrasting spots"
	   "solid    = solid color without pattern"
	   "If multiple patterns are observed, enter all"
	   "using the format ((feature 1.0) ...)"))
   (features (ring-around-neck upturned-nose black-head
	      black-tail)
	  ("Enter features observed about snake,"
	   "or UNK if unknown or absent"))
   (rattles nil "Does the snake have rattles on its tail?")
   (triangular-head nil
	  ("Does the snake have a triangular head,"
	   "noticeably larger than its neck?"))
   (red-and-yellow nil "Are red and yellow bands adjacent (touching)?")
   (cottonmouth nil ("Does the snake display an open mouth"
		     "that is white in color?"))
   (swims-head-out nil ("Does the snake swim with its head"
			"held above the water?"))
   (bitten nil "Has someone been bitten by this snake?")
   (fang-marks nil 
	  "Are two noticeably larger fang marks visible?")
   (identity (plains-blind-snake prairie-ringneck
     western-diamondback plains-blackhead 
     texas-coral-snake mexican-milk-snake))
   (environment (near-water in-water under-leaves grass woods)
	  "What is the environment where the snake was seen?")
   (behavior (aggressive playing-dead)
	  "Enter any unusual behaviors observed,"
	  "or UNK if none or unknown.")
   (poisonous nil)
   (latin-name string)
   )
 '(color size pattern)
 '(identity latin-name poisonous)
)

(defrules

;----------------------------------------------------------------
; First we have a lot of rules to conclude exact identities
; based on good data, assuming it is available.
;----------------------------------------------------------------

(rule01 ($and (same cntxt color pink)
	      (same cntxt size tiny)
	      (same cntxt environment under-leaves))
	(conclude cntxt identity plains-blind-snake tally 400))

(rule01a (same cntxt identity plains-blind-snake)
	 (conclude cntxt latin-name
		   "Leptotyphlops dulcis dulcis" tally 1000))

(rule05 ($and ($or (same cntxt color grey)
	           (same cntxt color brown))
	      (same cntxt size small)
	      (same cntxt features ring-around-neck))
        (conclude cntxt identity prairie-ringneck tally 800))

(rule05a (same cntxt identity prairie-ringneck)
         (conclude cntxt latin-name
         	   "Diadophis punctatus arnyi" tally 1000))

(rule07 ($and (same cntxt color tan)
              (same cntxt size tiny)
              (same cntxt environment under-leaves))
        (conclude cntxt identity flathead-snake tally 400))

(rule07a (same cntxt identity flathead-snake)
	 (conclude cntxt latin-name
		   "Tantilla gracilis" tally 1000))

(rule08 ($and (same cntxt color tan)
	      (same cntxt size small)
	      (same cntxt features black-head))
	(conclude cntxt identity plains-blackhead-snake
		  tally 800))

(rule08a (same cntxt identity plains-blackhead-snake)
	 (conclude cntxt latin-name
		   "Tantilla nigriceps fumiceps" tally 1000))

(rule13 ($and (same cntxt color brown)
	      ($or (same cntxt size tiny)
		   (same cntxt size small))
	      (same cntxt pattern stripes)
	      (same cntxt pattern speckles))
	(conclude cntxt identity texas-brown-snake tally 800))

(rule13a (same cntxt identity texas-brown-snake)
	 (conclude cntxt latin-name
		   "Storeria dekayi texana" tally 1000))

(rule16 ($and (same cntxt color tan)
	      (same cntxt size tiny)
	      (same cntxt pattern stripes)
	      (same cntxt pattern speckles))
	(conclude cntxt identity texas-lined-snake tally 500))

(rule16a (same cntxt identity texas-lined-snake)
	 (conclude cntxt latin-name
		   "Tropidoclonion lineatum texanum"
		   tally 1000))

(rule19 ($and (same cntxt color tan)
	      (same cntxt color white)
	      ($or (same cntxt size tiny)
		   (same cntxt size small))
	      (same cntxt pattern bands))
	(conclude cntxt identity ground-snake tally 600))

(rule19a (same cntxt identity ground-snake)
	 (conclude cntxt latin-name
		   "Sonora semiannulata" tally 1000))

(rule20 ($and (same cntxt color pink)
	      (same cntxt size tiny)
	      (same cntxt environment grass))
	(conclude cntxt identity western-smooth-earth-snake
		  tally 400))

(rule20a (same cntxt identity western-smooth-earth-snake)
	 (conclude cntxt latin-name
		   "Virginia valeriae elegans" tally 1000))

(rule21 ($and (same cntxt color brown)
	      (same cntxt size tiny))
	(conclude cntxt identity rough-earth-snake
		  tally 300))

(rule21a (same cntxt identity rough-earth-snake)
	 (conclude cntxt latin-name
		   "Virginia striatula" tally 1000))

(rule50 ($and ($or (same cntxt color tan)
		   (same cntxt color yellow))
	      (same cntxt color brown)
	      ($or (same cntxt pattern blotches)
		   (same cntxt pattern diamonds))
	      (same cntxt size large)
	      (notsame cntxt triangular-head yes)
	      (notsame cntxt rattles yes))
	(conclude cntxt identity bullsnake tally 600))

(rule50a (same cntxt identity bullsnake)
	 (conclude cntxt latin-name
		   "Pituophis melanoleucus sayi" tally 1000))

(rule70 ($and (same cntxt color black)
	      (same cntxt size large))
	(conclude cntxt identity texas-indigo-snake
		  tally 600))

(rule70a (same cntxt identity texas-indigo-snake)
	 (conclude cntxt latin-name
		   "Drymarchon corais erebennus" tally 1000))

(rule71 ($and (same cntxt color brown)
	      ($or (same cntxt size medium)
		   (same cntxt size small))
	      ($or (same cntxt environment in-water)
		   (same cntxt environment near-water))
	      (notsame cntxt thickness heavy)
	      (notsame cntxt cottonmouth yes)
	      (notsame cntxt swims-head-out yes))
	(conclude cntxt identity blotched-water-snake tally 400))

(rule71a (same cntxt identity blotched-water-snake)
	 (conclude cntxt latin-name
		   "Nerodia erythrogaster transversa" tally 1000))

(rule90 ($and (same cntxt color red)
	      (same cntxt color yellow)
	      (same cntxt color black)
	      (notsame cntxt red-and-yellow yes))
        (conclude cntxt identity mexican-milk-snake tally 800))

(rule90a (same cntxt identity mexican-milk-snake )
	 (conclude cntxt latin-name
		   "Lampropeltis triangulum annulata" tally 1000))

(rule96 ($and (same cntxt color red)
	      (same cntxt color yellow)
	      (same cntxt color black)
	      (same cntxt red-and-yellow yes))
        (conclude cntxt identity texas-coral-snake tally 1000))

(rule96a (same cntxt identity texas-coral-snake)
	 (conclude cntxt latin-name
		   "Micrurus fulvius tenere" tally 1000))

(rule97 ($and (same cntxt color brown)
	      ($or (same cntxt size medium)
		   (same cntxt size large))
	      ($or (same cntxt environment in-water)
		   (same cntxt environment near-water))
	      ($or (same cntxt thickness heavy)
		   (same cntxt cottonmouth yes)
		   (same cntxt swims-head-out yes)))
	(conclude cntxt identity water-moccasin tally 600))

(rule97a (same cntxt identity water-moccasin)
	 (conclude cntxt latin-name
		   "Agkistrodon piscivorus leucostoma" tally 1000))

(rule99 ($and (same cntxt color brown)
	      (same cntxt color tan)
	      ($or (same cntxt pattern bands)
		   (same cntxt pattern blotches))
	      ($or (same cntxt size small)
		   (same cntxt size medium))
	      (same cntxt environment woods)
	      (same cntxt triangular-head yes))
	(conclude cntxt identity copperhead tally 800))

(rule99a (same cntxt identity copperhead)
	 (conclude cntxt latin-name
		   "Agkistrodon contortrix laticinctus"
		   tally 1000))

(rule101 ($and ($or (same cntxt size medium)
		    (same cntxt size large))
	       (same cntxt color brown)
	       ($or (same cntxt pattern diamonds)
		    (same cntxt pattern blotches))
	       ($or (same cntxt rattles yes)
		    (same cntxt triangular-head yes)))
	 (conclude cntxt identity western-diamondback tally 800))

(rule101a (same cntxt identity western-diamondback)
	 (conclude cntxt latin-name
		   "Crotalus atrox" tally 1000))

(rule300 ($or (same cntxt identity western-diamondback)
	      (same cntxt identity rattlesnake)
	      (same cntxt identity texas-coral-snake)
	      (same cntxt identity water-moccasin)
	      (same cntxt identity copperhead))
	 (conclude cntxt poisonous yes tally 1000))

;----------------------------------------------------------------
; In some cases, good data is not available, but we would still
; like to conclude some useful information.
;----------------------------------------------------------------

; None of the striped or spotted snakes in this area are poisonous.
(rule401 ($or (same cntxt pattern stripes)
	      (same cntxt pattern spots))
	 (conclude cntxt poisonous yes tally -600))

(rule402 ($and (notsame cntxt identity western-diamondback)
	       (notsame cntxt identity texas-coral-snake)
	       (notsame cntxt identity water-moccasin)
	       (notsame cntxt identity copperhead)
	       (notsame cntxt identity rattlesnake))
	 (conclude cntxt poisonous yes tally -300))

(rule403 ($and (same cntxt rattles yes)
	       (notsame cntxt size large))
	 (conclude cntxt identity rattlesnake tally 800))

(rule403a ($and (same cntxt identity rattlesnake)
		(same cntxt triangular-head yes))
	  (conclude cntxt latin-name
		    "Crotalus" tally 1000))

(rule403b ($and (same cntxt identity rattlesnake)
		(thoughtnot cntxt triangular-head yes))
	  (conclude cntxt latin-name
		    "Sistrurus" tally 1000))

(rule404 ($and (same cntxt bitten yes)
	       (same cntxt fang-marks yes))
	 (conclude cntxt poisonous yes tally 201))

(rule405 ($and (same cntxt bitten yes)
	       (notsame cntxt fang-marks yes))
	 (conclude cntxt poisonous yes tally -300))
)
