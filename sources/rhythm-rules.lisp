;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;;; *************************************************************
;;; Copyright (C) 2013 Torsten Anders (torsten.anders@beds.ac.uk) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; *************************************************************


(in-package :cluster-rules) 


;;;
;;; General defs
;;;

(ccl::add-box-type :rule-type-mbox 
		   ;; TODO: this is probably an unnecessary nesting of ccl::mk-menu-subview
		   `(ccl::mk-menu-subview :menu-list ,(ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")) :value 1))


;; TODO: Better idea: I could define a var for every RGB value, e.g., for all rhythm values, to edit these colours later at a single place
;; ;; This was a test to abstract keyword params of PWGLDef, but it is not quite working -- the keyword param list is not evaluated, and hence I cannot use a variable for it.
;; (defparameter rhythm-rules-colour 
;;   ;; slightly orange colour (perhaps different colour for rhythm and harmony rules?)
;;   ;; TODO: currently too light (connections hardly seen)
;;   '(:r 0.9 :g 0.8 :b 0.6) 
;;   "Keyword parameters for PWGLDef")


;;;
;;; Aux
;;;

(defun is-rest? (dur) 
  "dur is a rest -- a negative number"
  (< dur 0))

(defun is-note? (dur) 
  "dur is a note -- a positive number"
  (> dur 0))

(defun is-grace-note? (dur) 
  "dur is a grace note with duration ="
  (= dur 0))


;;;
;;; Rhythmic rules/constraints
;;;

;; rhythm-profile-BPF-hr

;; TODO: rests can occur at any position of a note
(PWGLDef rhythm-profile-BPF-hr 
	 ((voices 0)
	  (n 0)
	  (BPFs NIL)
	  (min-scaling 1/16)
	  (max-scaling 1)
	  &key
	  (rnd-deviation 0)
	  (permutate #'identity))
	 "Heuristic constraint: rhythmic values essentially follow a BPF. However, the BPF is slightly processed. Firstly, the BPF values are scaled into the interval [min-scaling, max-scaling]. Secondly, the BPF can be somewhat randomised (amount chosen with rnd-deviation, see below). Also, the BPF becomes somewhat 'curved' (using power 3) to address the distribution of rhythmic values (e.g., 1/16, 1/8, 1/4..) [the latter is a HACK].  

Note that the rule follow-profile-hr is more flexible than the rule rhythm-profile-BPF-hr, but this rule is more easy to use for its purposes. Also, this rule allows for rests to occur at any position of a note (with the same duration).

Args:
  voices (int or list of ints): the voice(s) to which the constraint is applied.
  n (int): number of notes
  BPFs (a BPF or list of BPFs): the BPF to follow. 
  min-scaling (positive number): min dur (e.g, 1/16)
  max-scaling (positive number): max dur

Keyword args:
  rnd-deviation (float): amount by which the resulting value for the heuristic deviates from given BPF. 0 means no deviation, 0.5 means the value may deviate up to 50 percent (to either side).
  permutate (a function): arbitrary permutations of the BPF can be defined by a function expecting a list of numbers and returning a list of numbers of the same length. Such permutations are applied after all internal processing of the BPF. 

NOTE: This rule can apply different BPFs to different voices with different settings. If a list of BPFs is given, then a different BPF is given to each of the voices listed. In that case, all other arguments (except n) can be either single values that are shared by all voices, or a list of different values for the different voices.
"
	 (:groupings '(3 2))
	 (let ((l (length (if (listp BPFs) BPFs (list BPFs)))))
	   (mappend #'(lambda (BPF voice min-scaling max-scaling rnd-deviation permutate)
			;; (format T "rhythm-profile-BPF-hr: args: ~A ~%" 
			;; 	(list BPF voice min-scaling max-scaling rnd-deviation permutate))
			(let* ((BPF-xs (pw::g-scaling (pw::g-power (pw::g-scaling (ccl::pwgl-sample BPF n) 
										  0 1) 
								   3)
						      min-scaling max-scaling))
			       (abs-rnd-deviation (abs rnd-deviation))
			       (rnds (loop for i from 1 to n
					   collect (pw::g-random (* abs-rnd-deviation -1) abs-rnd-deviation)))
			       (BPF-rnd-xs (funcall permutate (pw::g+ BPF-xs (pw::g* BPF-xs rnds)))))
			  (hr-rhythms-one-voice #'(lambda (xs) 
						    "Returns a heuristic -- better BPF matches are preferred. Essentially, returns the abs difference between current dur and corresponding env value."
						    (format T "rhythm-profile-BPF-hr: xs: ~A, BPF-rnd-xs: ~A ~%" 
							    xs BPF-rnd-xs)
						    (- 1000 (* (abs 
								;; abs: both rests and note can occur
								(- (abs (first (last xs))) 
								   (abs (nth (1- (length xs)) 
									     BPF-rnd-xs)))) 
							       100)))		       
						voice
						:list-with-all-durations)))
		    (if (listp BPFs) BPFs (list BPFs))
		    ;; if only a single voice but multiple BPFs are given, 
		    ;; then only the given voice is constrained with the 1st BPF
		    (if (listp voices) voices (list voices)) 
		    (if (listp min-scaling) min-scaling (make-list l :initial-element min-scaling))
		    (if (listp max-scaling) max-scaling (make-list l :initial-element max-scaling))
		    (if (listp rnd-deviation) rnd-deviation (make-list l :initial-element rnd-deviation))
		    (if (listp permutate) permutate (make-list l :initial-element permutate)))
		 ))



;; no-two-consecutive-syncopations

(defun no-two-consecutive-syncopations-rule (offs1 offs2)
  "For any two consecutive beats, at least one notes must start on a beat (has an offset = 0). Constraint intended for r-meter-note applied to beats with input model :offset."
  (or (= offs1 0)
      (= offs2 0)))

;; BUG: 
;; - not working? 
;; - prevents rests?
(PWGLDef no-two-consecutive-syncopations 
	 ((voices 0)
	  (metric-structure () (ccl::mk-menu-subview :menu-list '(":beats" ":1st-beat")))
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "For any two consecutive beats/bars, at least one notes must start on a beat. All arguments are inherited from r-meter-note."
	 () 
	 (r-meter-note #'no-two-consecutive-syncopations-rule
		       voices
		       metric-structure
		       :offset
		       :norm
		       rule-type weight))


;; no-syncopation

(defun no-syncopation-rule (offs)
  "Returns function (constraint). On the given beats a note must start. Constraint intended for r-meter-note applied to either 1st beats of bars or beats with input model offs."
  (= offs 0))

(PWGLDef no-syncopation 
	 ((voices 0)
	  (metric-structure () (ccl::mk-menu-subview :menu-list '(":beats" ":1st-beat")))
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "For any two consecutive beats/bars, at least one notes must start on a beat. All arguments are inherited from r-meter-note."
	 ()
	 (r-meter-note #'no-syncopation-rule
		       voices
		       metric-structure
		       :offset
		       :norm
		       rule-type weight))


;; only-simple-syncopations

(defun is-syncopation (dur offs)
  "[Aux def] Returns boolean whether or not the note starting at offs1 constitutes a syncopation, i.e., whether it crosses a reference offset 0.
Note: long notes that exceed the duration of a beat and start on a beat are not recognised."
  (and (/= offs 0)
       (> dur (* offs -1))))

;; TODO: consider allowing to set the complexity with an argument
;; NOTE between beat points I allow for arbitrary durations (e.g., 1/4 within a triplet) Do I want to restrict that? I could also do that with motif defs (more restrictive on resulting rhythms, but possibly all I need)
(defun only-simple-syncopations-rule (d_offs)
  "Restricts syncopations over beats to certain relatively simple cases. For example, the only possible syncopation allowed for a note value 1/4 is 1/8 before a beat.  
Intended for r-note-meter with format d_offs on beats."
  (let ((dur (first d_offs))
        (offs (second d_offs)))
    (if (is-syncopation dur offs)
        (let (; (dur-denom (denominator dur))
              (dur-num (numerator dur))
					; (offs-denom (denominator offs))
              )
	  #|          (format t "only-matching-tuplets: dur: ~A offs: ~A result: ~A~%" 
	  dur offs
	  (cond ((= dur-num 1) ; undotted note values (including tuplets)
	  (= offs (* dur -1/2)))
	  ((= dur-num 3) ; notes with single dots
	  (= offs (* dur -2/3)))
	  (t nil)
	  )) |#
          (cond ((= dur-num 1) ; undotted note values (including tuplets)
                 (= offs (* dur -1/2)))
                ((= dur-num 3) ; notes with single dots
                 (= offs (* dur -2/3)))
                (t nil)
                ))
      #|
      (cond ((and (= (gcd dur-denom 2) 2) ;; cases like 1/8, 1/4
      (= dur-num 1))
                         ; (= (gcd offs-denom 2) 2) ;; too lenient ;
      (= offs (* dur -1/2)))
      ((and (= (gcd dur-denom 2) 2) ;; cases like 3/4
      (= dur-num 3))
      (= offs (* dur -2/3)))
      ((and (= (gcd dur-denom 3) 3) ;; undotted triplet values
      (= dur-num 1))
      (= offs (* dur -1/2)))
      (t nil)
      )
      |#
      t)))

(PWGLDef only-simple-syncopations 
	 ((voices 0)
	  (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Restricts syncopations over beats to certain relatively simple cases. For example, the only possible syncopation allowed for a note value 1/4 is 1/8 before a beat. All arguments are inherited from r-note-meter."  
	 ()
	 (r-note-meter #'only-simple-syncopations-rule
		       voices
		       :d_offs
		       :beats
		       :durations
		       gracenote-mode
		       rule-type weight))


;; only-simple-tuplet-offs-rule

(defun max-multiple (int &optional (multiple-candidates '(2 3 5 7)))
  "Aux def"
  (apply #'max (mapcar #'(lambda (x) (gcd int x)) multiple-candidates)))


(defun only-simple-tuplet-offs-rule  (d_offs)
  "Restricts the rhythmic position of notes to relatively simple cases. For example, triplet notes can only be part of a triplet.
Intended for r-note-meter with format d_offs on beats."
  (let ((offs (second d_offs)))
    (if (= offs 0)
        T
      (let* ((dur (first d_offs))         
             (dur-denom (denominator dur))
					; (dur-num (numerator dur))
             (offs-denom (denominator offs)))
        ;; (format t "only-simple-tuplet-offs dur: ~A offs: ~A result: ~A~%" 
        ;;         dur offs
        ;;         (= (max-multiple dur-denom) 
        ;;            (max-multiple offs-denom)))
        (= (max-multiple dur-denom) 
           (max-multiple offs-denom))
	#|
        (cond ((= (gcd offs-denom 5) 5)
	(= (gcd dur-denom 5) 5))
	((= (gcd offs-denom 3) 3)
	(= (gcd dur-denom 3) 3))
	((= (gcd offs-denom 2) 2)
	(= (gcd dur-denom 2) 2))
	(t nil)
	) |# 
	))))

(PWGLDef only-simple-tuplet-offs 
	 ((voices 0)
	  (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Restricts the rhythmic position of notes to relatively simple cases. For example, triplet notes can only be part of a triplet. All arguments are inherited from r-note-meter."  
	 ()
	 (r-note-meter #'only-simple-tuplet-offs-rule
		       voices
		       :d_offs
		       :beats
		       :durations
		       gracenote-mode
		       rule-type weight))


;; ;; TODO: 
;; ;; - likelyhood controlled with a BPF (with random offset)
;; (PWGLDef include-rests-hr ((voices 0)
;; 			   ()
;; 			   (rnd-deviation))
;; 	 ""
;; 	 () 
;; 	 (let* ((BPF-xs (pw::g-scaling (pw::g-power (pw::g-scaling (ccl::pwgl-sample BPF n) 
;; 								   0 1) 
;; 						    3)
;; 				       min-scaling max-scaling))
;; 		(abs-rnd-deviation (abs rnd-deviation))
;; 		(rnds (loop for i from 1 to n
;; 			    collect (pw::g-random (* abs-rnd-deviation -1) abs-rnd-deviation)))
;; 		(BPF-rnd-xs (funcall permutate (pw::g+ BPF-xs (pw::g* BPF-xs rnds)))))
;; 	 (hr-rhythms-one-voice #'(lambda (rhythm)
;; 				   ())
;; 			       voices
;; 			       :list-with-all-durations))


;; start-with-rest

(PWGLDef start-with-rest ((rest-dur 0)
			  (voices 0)
			  &optional
			  (rule-type  () :rule-type-mbox)
			  (weight 1))
	 "Start the given voice(s) with a rest of the given duration (either an int or a list of ints indicating a domain) If rest-dur is NIL then this means a rest of any duration is acceptable.

Hint: make sure you included rests in your rhythm domain (as negative integers). 

Other optional arguments are inherited from r-index-rhythms-one-voice."
	 () 
	 (r-index-rhythms-one-voice #'(lambda (rhythm)
					(and (< rhythm 0)
					     (or (not rest-dur) ; rest-dur is NIL
						 (member (abs rhythm) 
							 (mapcar #'abs
								 (if (listp rest-dur) rest-dur (list rest-dur)))))))
				    '(0)
				    voices
				    :position-for-duration
				    rule-type weight))




;; metric-offset-of-motif

(PWGLDef metric-offset-of-motif ((metric-offset 0)
				 (voices 0)
				 (metric-structure () (ccl::mk-menu-subview :menu-list '(":beats" ":1st-beat")))
				 (grid 1/4)
				 &optional
				 (min-motif-length NIL)
				 (rule-type  () :rule-type-mbox)
				 (weight 1))
	 "Motifs must start metric-offset away from set beat. Motifs starting with rests are not constrained.

Args:
  metric-offset (ratio): How far should motifs be shifted with respect to the metric-structure? For example, if metric-offset is -1/8, then motifs will be shifted to start an eighths note before the beat (or bar). 
  grid (ratio): Motifs could be longer than a beat (or bar) so that this rule would be checked more than once. If a motif is longer than the set grid, it will only be checked at its beginning whether its metric offset is as set. 

Optional arg:
  min-motif-length (int): motifs with a length below this setting are uneffected.

Other arguments are inherited from R-meter-note.
"
	 () 
	 (r-meter-note #'(lambda (offs_motif)
			   (let ((offs (first offs_motif))
				 (motif (second offs_motif)))
			     (if (and (> (first motif) 0) ; motif starting with note?
				      (and (not NIL)
					   (>= (length motif) min-motif-length)))
				 (or ;; is motif on set position?
				  (= (- offs metric-offset) 0)
				  ;; if checked per beat, motif is potentially longer than a single beat
				  (<= offs (* -1 grid)))
			       T)))
		       voices
		       metric-structure
		       :offset_motif
		       :norm
		       rule-type weight))


;; minimum-phrase-dur

;; TODO: 
;; - !! Efficiency: change internal BPF representation from list into array/vector for faster access during search -- see PWConstraints example.. 
;; - OK Make min-phrase-length controllable by a BPF
;; - OK Consider introducing min/max/exactly switch for the phrase-length
;; - OK Allow for several rests to occur consecutively
;; - NO Consider interface change: have optional settings for min and max
(PWGLDef phrase-length ((phrase-length 0)
			;; (skipped-durs-no 0)
			(relation  () (ccl::mk-menu-subview :menu-list '(":min" ":max")))
			(voices 0)
			&optional
			(n 1)
			(rule-type  () :rule-type-mbox)
			(weight 1))
	 "This rule controls the number of notes and grace notes between rests (the length of phrases). 

Args:
  phrase-length (int): The set number of notes between rests. Consecutive rests (effectively longer rests) can occur freely.
  relation: Whether the set phrase length is the required minimum or maximum. (If you want to constrain both the upper and lower boundary then simply use two of these rules.)

BUG: Strangely, at least one motif of the rhythm domain must have at least length 2. Not yet sure why..

Other arguments are inherited from R-rhythms-one-voice
"
	 () 
	 (let ((BPF-vals (if (ccl::break-point-function-p phrase-length)
			     (ccl::pwgl-sample phrase-length n)
			   phrase-length)))
	   (R-rhythms-one-voice 
	    #'(lambda (durs)
		(let ((curr-phrase-length (if (ccl::break-point-function-p phrase-length)
					      (nth (length durs) BPF-vals)
					    phrase-length))
		      (rev-durs (reverse durs)))
		  (if (second rev-durs) ; there are at least two items
		      (case relation
			(:min (if (and (is-rest? (first rev-durs)) 
				       (not (is-rest? (second rev-durs)))) ; rests can follow each other
				  (let ((prev-rest-pos (position-if #'is-rest? (rest rev-durs))))
				    (if prev-rest-pos ; there has been a rest before
					(<= curr-phrase-length prev-rest-pos) ; tested on rest
				      (<= curr-phrase-length (length rev-durs))))
				T))
			(:max (if (and (not (is-rest? (first rev-durs))))
				  (let ((prev-rest-pos (position-if #'is-rest? (rest rev-durs))))
				    (if prev-rest-pos ; there has been a rest before
					(>= (1- curr-phrase-length) prev-rest-pos) ; tested on note
				      (>= curr-phrase-length (length rev-durs))))
				T))
			))))	
	    ;; (let ((rev-durs (reverse durs)))
	    ;; 	(if (and (second rev-durs) ; there are at least two items
	    ;; 		 ;; (case relation
	    ;; 		 ;;   (:min (and 
	    ;; 		 ;; 	  (< (first rev-durs) 0) ; current item is a rest
	    ;; 		 ;; 	  (> (second rev-durs) 0))) ; previous dur is not rest
	    ;; 		 ;;   (:max (> (first rev-durs) 0))) ; current item is a note
	    ;; 		 (< (first rev-durs) 0) ; current item is a rest
	    ;; 		 (> (second rev-durs) 0) ; previous dur is not rest
	    ;; 		 ) 
	    ;; 	    (let ((prev-rest-pos (position-if #'(lambda (x) (< x 0))
	    ;; 					      (rest rev-durs))))
	    ;; 	      ;; TODO: revise case where that have not been rests befoer
	    ;; 	      (if prev-rest-pos ; there has been a rest before
	    ;; 		  (funcall (case relation
	    ;; 			     (:min #'>=)
	    ;; 			     (:max #'<=)
	    ;; 			     ;; (:equal #'=)
	    ;; 			     )						       
	    ;; 			   prev-rest-pos phrase-length)
	    ;; 		T))
	    ;; 	  T))
	    voices
	    :all-durations
	    rule-type weight)))


;; TODO: if future versions of R-rhythm-rhythm allow, then allow max-factor to be a BPF that can change over time (e.g., that way, some parts of the music can be forced to be strictly homophonic, while others are not) 
(PWGLDef similar-sim-durations ((voices '(0 1))
				(max-factor 1)
				(rest-mode () (ccl::mk-menu-subview :menu-list '(":constrain" ":ignore")))
				&optional
				(rule-type  () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
				(weight 1))
	 "This rule restricts the maximum difference between simultaneous note durations. Together with the rule r-rhythm-hierarchy (from library cluster engine) this rule allows to enforce a homophonic texture and also almost homophonic textures. 

Note that grace notes are ignored by this rule.

Args:
  voices (a list of ints): The list of voices affected by this constraint. The first given voice is used as a reference: notes of all other voices are compared to this voice. NOTE: if the reference voice is not the voice with the lowest number, then the search is slowed down.
  max-factor (a ratio): If max-factor is 1, then the simultaneous durations always have exactly the same duration (however, neither the size of the duration nor whether they start together is constrained). If max-factor is larger (or smaller) than one then this factor defines the largest possible quotient between simultaneous durations. For example, if max-factor is 2 then any note can be at most the double and at least halve of the simultaneous note (note that the rule behaves the same whether max-factor is 1/2 or 2).
  rest-mode: Whether or not to also constrain rests or not. If rests are constrained, then all simultaneous notes must be notes and simultaneous rests must be rests.

Other args are inherited from R-rhythm-rhythm.

BUG: Arg factor seemingly not fully working as documented yet if factor > 1.
"
	 ()
	 (let ((voice1 (first voices)))
	   (mapcar #'(lambda (voice2)
		       (R-rhythm-rhythm #'(lambda (d1_offset_d2)
					    (destructuring-bind (dur1 offset dur2) d1_offset_d2
					      (let ((both-notes-or-rests?  
						     (case rest-mode
						       (:ignore T)
						       (:constrain (= (signum dur1) (signum dur2)))))) 
						(and both-notes-or-rests?
						     (or (= dur1 dur2)
							 (and (< dur1 dur2) (<= dur2 (* dur1 max-factor)))
							 (and (< dur2 dur1) (<= dur1 (* dur2 max-factor))))))))
					voice1
					voice2
					:d1_offs_d2
					:norm
					(case rest-mode
					  (:ignore :at-durations-v1)
					  (:constrain :at-events-v1))
					rule-type
					weight))
		   (rest voices))))


;;;
;;; Accent Model 
;;;

#|
;; Musing
Accent-rules must be applied to different score contexts. So, I would need to combine r-note-meter calls with different args (e.g., different format settings and different number of args per rule). However, it will then be difficult to combine the rating of the applied constraints. I basically need reified constraints of the cluster-engine, and likely an additional variable to store the rating. Hm...

What I could easily do is apply multiple accent rules independently. However, I think a strength of the Strasheela accent model is that it allows to combine multiple accent constraints that depend on each other.  


;; TODO:
- Add missing args of the Strasheela def
- Write documentation

;; (PWGLDef accent-if ((accent-rules NIL)
;; 		    (voices 0)
;; 		    (metric-position () (ccl::mk-menu-subview :menu-list '(":1st-beat" ":beats")))
;; 		    (min-rating 1)
;; 		    (strictness () (ccl::mk-menu-subview :menu-list '(":note" ":position" ":note-n-position")))
;; 		    (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
;; 		    &optional
;; 		    (rule-type  () :rule-type-mbox)
;; 		    (weight 1)
;; 		    ;; min-rating
;; 		    ;; strictness
;; 		    )
;;   "

;; "
;;   ()
;;   (r-note-meter #'(lambda (d_offs1 d_offs2 d_offs3)
;; 		    )
;; 		voices
;; 		:d_offs
;; 		metric-structure
;; 		:durations
;; 		gracenote-mode
;; 		rule-type weight))

|#

;;;
;;; Accent model: top-level definitions
;;;

;; TODO: for metric-accents and accents-in-other-voice
;; 
;; - Allow for different working modes, defined in Strasheela as strictness
;;   (strictness () (ccl::mk-menu-subview :menu-list '(":note" ":position" ":note-n-position")))
;; - Define set of accent-rules
;; - Fix problems of accent-longer-than-predecessor-ar with first/last notes
;; - Try combining accent constraints, where either accent constraint can be met
;; - Decide: should different accent rules have access to different information (different format settings: :offs, :d_offs, :d_offs_m, :d_offs_m_n)? Perhaps you better defined different boxes for such cases? For now assume the same setting for all
(PWGLDef metric-accents
	 ((voices 0)
	  (metric-structure () (ccl::mk-menu-subview :menu-list '(":1st-beat" ":beats")))
	  (accent-rule () (ccl::mk-menu-subview :menu-list '(":longer-than-predecessor"
							     ":longer-than-neighbours")))
	  (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Restricts where metric accents occur depending on the underlying meter. 

If an accent occurs, then it is on the position defined. 

Args:
  metric-structure: position where accents are controlled (on any beat or the first beat of a measure).

  accent-rule (menu item or function): A function returning true if an accent is expressed and nil otherwise. The function expects one of more arguments, all in the form (dur offs), where dur is the duration of a note and offs is the offset to the following accent (i.e. the duration until the following accent). Example: '(1/4 -1/8) 

  Some accent rules are predefined and can be simply selected in the menu of the argument. 
    :longer-than-predecessor: Accented notes are longer than the preceeding note and at least as long as the succeeding note. BUG: not constrained for first and last 2 notes! (fixing that needs more flexible rule applicators)
    :longer-than-neighbours: Accented notes are longer than the preceeding and the succeeding note. BUG: not constrained for first and last 2 notes! 

  Other predefined accent rules expect additional arguments controlling their effect. These are available under the Cluster Rules sub menu rhythm - accent rules.

Other arguments are inherited from r-note-meter.
" 
	 ()
	 (r-note-meter (case accent-rule
			 (:longer-than-predecessor #'accent-longer-than-predecessor-ar)
			 (:longer-than-neighbours #'accent-longer-than-neighbours-ar)
			 (otherwise accent-rule))
		       voices
		       :d_offs
		       metric-structure
		       :incl-rests
		       gracenote-mode
		       rule-type weight))


(PWGLDef accents-in-other-voice
	 ((voices 0)
	  (accent-rule () (ccl::mk-menu-subview :menu-list '(":longer-than-predecessor"
							     ":longer-than-neighbours")))
	  &optional
	  (accents-voice 3)
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Restricts where metric accents occur depending on the note onsets defined in an 'accents voice'.

If an accent occurs, then it is on the position defined. 

Args:
  voices (int or list of ints): the numbers of voice(s) to constrain.

  accent-rule (menu item or function): A function returning true if an accent is expressed and nil otherwise. The function expects one of more arguments, all in the form (dur offs), where dur is the duration of a note and offs is the offset to the following accent (i.e. the duration until the following accent). Example: '(1/4 -1/8). A note is 'on' the accent if its offset = 0. 
Some accent rules are predefined and can be simply selected in the menu of the argument. Other predefined accent rules expect additional arguments controlling their effect. These are available under the Cluster Rules sub menu rhythm - accent rules.

Optional args:

  accents-voice: the number of the voice that defines accents. Each note onset in accents-voice is taken as an accent for the given voices.

Other arguments are inherited from r-rhythm-rhythm.
" 
	 ()
	 (mapcar #'(lambda (voice)
		     (r-rhythm-rhythm (case accent-rule
					(:longer-than-predecessor #'accent-longer-than-predecessor-ar)
					(:longer-than-neighbours #'accent-longer-than-neighbours-ar)
					(otherwise accent-rule))
				      voice
				      accents-voice
				      :d1_offs
				      :norm
				      :at-durations-v1				    
				      ))
		 (if (listp voices) voices (list voices))))

;;;
;;; Accent model: predefined accent rules
;;;

;; !! TODO: later allow for actual accent constraints to be given as args with fun expecting funs and returning a fun
;; TODO: 
;; - refine model: all notes beyond a certain duration are also accented -- define this with extra rule..
;; - how can I take rests into account?
;; - rule only applied when there are actually three notes in succession, i.e., not to 1st two notes. In Strasheela, rule is applied in such a way that values of 1st two "args" of coresponding function can be nil
;;
;; NOTE: constraint applied to d_offs2, but only checked after d_offs3 is bound
(defun accent-longer-than-predecessor-ar  (d_offs1 d_offs2 d_offs3)
  "Accent rule for metric-accents or accents-in-other-voice. Accented notes are longer than the preceeding note and at least as long as the succeeding note."
  (destructuring-bind ((dur1 offs1) (dur2 offs2) (dur3 offs3)) (list d_offs1 d_offs2 d_offs3)
    (if (every #'plusp (list dur1 dur2 dur3)) ; no rests 
	(let ((accent-rating2 (and (< dur1 dur2) (>= dur2 dur3))))
	  ;; If note is accented then start on what is set at metric-structure, e.g., on beat (but there can be beats etc. without accent)
	  (if accent-rating2
					; (> accent-rating2 0) ;; use when generalised later...
	      (= offs2 0) 
	    T))
      T)))

(defun accent-longer-than-neighbours-ar  (d_offs1 d_offs2 d_offs3)
  "Accent rule for metric-accents or accents-in-other-voice. Accented notes are longer than the preceeding and the succeeding note."
  (destructuring-bind ((dur1 offs1) (dur2 offs2) (dur3 offs3)) (list d_offs1 d_offs2 d_offs3)
    (if (every #'plusp (list dur1 dur2 dur3)) ; no rests 
	(let ((accent-rating2 (< dur1 dur2 dur3)))
	  ;; If note is accented then start on what is set at metric-structure, e.g., on beat (but there can be beats etc. without accent)
	  (if accent-rating2
					; (> accent-rating2 0) ;; use when generalised later...
	      (= offs2 0) 
	    T))
      T)))

(PWGLDef mk-accent-has-at-least-duration-ar ((min-duration 1/4))
  "Returns an accent rule for metric-accents or accents-in-other-voice. Accented notes are at least min-duration long."
  ()
  #'(lambda (d_offs)
      (destructuring-bind (dur offs) d_offs
	(if (plusp dur) ; no rest
	    (let ((accent-rating (>= dur min-duration)))
	      (if accent-rating
		  (= offs 0) 
		T))
	  T))))


;; Unused -- just test for possible future development, depending on development of the Cluster Engine itself
;; BUG: test case that does not work -- see comment at accent-strictness 
(PWGLDef mk-accent-has-at-least-duration-ar_test-with-strictness ((strictness () (ccl::mk-menu-subview :menu-list '(":note" ":position" ":note-n-position")))
					     (min-duration 1/4))
  "Returns an accent rule for metric-accents or accents-in-other-voice. Accented notes are at least min-duration long."
  ()
  #'(lambda (d_offs)
      (destructuring-bind (dur offs) d_offs
	(if (plusp dur) ; no rest
	    (let ((accented-note? (>= dur min-duration))
		  (on-position? (= offs 0)))
	      (accent-strictness strictness accented-note? on-position?))
	  T))))


;; Unused -- just test for possible future development, depending on development of the Cluster Engine itself
;; BUG: for strictness :position this must be applied for all notes, not just the notes on the selected position. However, then I need to get access to metric position (i.e., on start of measure) within rule. However, R-note-meter does not support giving such information, so I would loose the argument metric-structure for the rule metric-accents.
(defun accent-strictness (strictness accented-note? on-position?)
  "Controls which constrain is applied between accented-note? (Boolean whether note follows accent constraint) and on-position? (Boolean whether note is on the position of a metric accent). 

Arg strictness is a keyword switching bewtween three cases.
  :note  -- any accented note is situated on a metric accent (but there can be metric accents without a note)
  :position -- any metric accent has an accented note (but there can accented notes also elsewhere)
  :note-n-position -- any accented note is situated on a metric accent and any metric accent has an accented note 
"
  (case strictness
    (:note (if accented-note? on-position? T))
    ;; BUG: on-position? T if note is starting with any beat, but must not be the "current" beat
    (:position (if on-position? accented-note? T))
    (:note-n-position (and on-position? accented-note?))))


;; ;;
;; ;; First simplified draft
;; ;; TODO: Remove once more general model is defined
;; ;;


;; #|
;; Strasheela doc from IsLongerThanPredecessor
;; Note N is longer than the preceeding note and not shorter than succeeding note (duration + offsetTime used for calculating the perceived duration). If a preceeding or succeeding note does not exist (in the same temporal container) then the constraint returns 0.

;; If note is accented then start on a beat OR if not starts on a beat then it most accented.
;; |#

;; ;; !! TODO: later allow for actual accent constraints to be given as args with fun expecting funs and returning a fun
;; ;; TODO: 
;; ;; - refine model: all notes beyond a certain duration are also accented -- define this with extra rule..
;; ;; - how can I take rests into account?
;; ;; - rule only applied when there are actually three notes in succession, i.e., not to 1st two notes. In Strasheela, rule is applied in such a way that values of 1st two "args" of coresponding function can be nil
;; ;;
;; ;; NOTE: constraint applied to d_offs2, but only checked after d_offs3 is bound
;; (defun accent-is-longer-than-predecessor-rule  (d_offs1 d_offs2 d_offs3)
;;   "Strait-forward but unflexible accent model implementation."
;;   (destructuring-bind ((dur1 offs1) (dur2 offs2) (dur3 offs3)) (list d_offs1 d_offs2 d_offs3)
;;     (if (every #'plusp (list dur1 dur2 dur3)) ; no rests 
;; 	(let ((accent-rating2 (and (< dur1 dur2) (>= dur2 dur3))))
;; 	  ;; If note is accented then start on what is set at metric-structure, e.g., on beat (but there can be beats etc. without accent)
;; 	  (if accent-rating2
;; 					; (> accent-rating2 0) ;; use when generalised later...
;; 	      (= offs2 0) 
;; 	    T))
;;       T)))

;; (defun accent-has-at-least-duration-rule (d_offs)
;;   (destructuring-bind (dur offs) d_offs
;;     (if (plusp dur) ; no rest
;; 	(let ((accent-rating (>= dur min-duration)))
;; 	  ;; If note is accented then start on what is set at metric-structure, 
;; 	  ;; e.g., on beat (but there can be beats etc. without accent)
;; 	  (if accent-rating
;; 	      (= offs 0) 
;; 	    T))
;;       T)))


;; ;; TODO: 
;; ;; - generalise to allow also a predefined accent structure in a voice with a fixed rhythm. See example TODO/accent-model-over-rhythm-voice.pwgl
;; ;; - Allow for different strictness settings: (":note" ":position" ":note-n-position")
;; ;; - combine with other accent constraints into single def, where rules can be selected
;; ;; 
;; ;; BUG:
;; ;; - Random accents can happen on the 1st or last two notes. -- Fix by additional index rule applications to those notes. (then remove bug notice in doc string)
;; (PWGLDef accent-is-longer-than-predecessor 
;; 	 ((voices 0)
;; 	  (metric-structure () (ccl::mk-menu-subview :menu-list '(":1st-beat" ":beats")))
;; 	  (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
;; 	  &optional
;; 	  (rule-type  () :rule-type-mbox)
;; 	  (weight 1))
;; 	 "Strait-forward but unflexible accent model implementation.
;; If an accent occurs, then it is on the position defined. 

;; Notes on the selected metric position (metric-structure, either :beats or :1st-beat) are rhythmically accented: such note is longer than the preceeding note and not shorter than the succeeding note.

;; All arguments are inherited from r-note-meter.

;; Bugs:
;; - Random accents can happen on the 1st or last two notes. 
;; " 
;; 	 ()
;; 	 (r-note-meter #'accent-is-longer-than-predecessor-rule
;; 		       voices
;; 		       :d_offs
;; 		       metric-structure
;; 		       :incl-rests
;; 		       gracenote-mode
;; 		       rule-type weight))


;; ;; TODO:
;; ;; - see comments for accent-is-longer-than-predecessor 
;; (PWGLDef accent-has-at-least-duration
;; 	 ((voices 0)
;; 	  (min-duration 1/4)
;; 	  (metric-structure () (ccl::mk-menu-subview :menu-list '(":1st-beat" ":beats")))
;; 	  (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
;; 	  &optional
;; 	  (rule-type  () :rule-type-mbox)
;; 	  (weight 1))
;; 	 "Strait-forward but unflexible accent model implementation.
;; If an accent occurs, then it is on the position defined. 

;; Notes on the selected metric position are rhythmically accented by having at least the set min-duration.

;; Other arguments are inherited from r-note-meter.
;; " 
;; 	 ()
;; 	 (r-note-meter #'accent-has-at-least-duration-rule
;; 		       voices
;; 		       :d_offs
;; 		       metric-structure
;; 		       :incl-rests
;; 		       gracenote-mode
;; 		       rule-type weight))




#|

;;;
;;; Patterns
;;;

;; Every 6th variable must be equal, resulting in a cycle of length 6
;; TODO: generalise with a higher-order fun (a bit challenging to dynamically create the lambda list, perhaps with a new macro?)
#'(lambda (x1 x2 x3 x4 x5 x6)
(= x1 x6)) 

|#


