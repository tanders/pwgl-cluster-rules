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
;;; Rhythmic rules/constraints
;;;

;; rhythm-profile-BPF-hr

(PWGLDef rhythm-profile-BPF-hr 
	 ((voices 0)
	  (BPF NIL)
	  (n 0)
	  (min-scaling 1/16)
	  (max-scaling 1)
	  (rnd-deviation 0.3))
	 "Heuristic constraint: rhythmic values essentially follow a BPF. However, the BPF is slightly processed. Firstly, it can be somewhat randomised (amount chosen with rnd-deviation, see below). Secondly, the BPF values are scaled into the interval [min-scaling, max-scaling]. Also, the BPF becomes somewhat 'curved' (using power 3) to address the distribution of rhythmic values (e.g., 1/16, 1/8, 1/4..) [the latter is a HACK].  

Args:
voices (int or list of ints): the voice(s) to which the constraint is applied.
n (int): number of notes/durs
BPF (a BPF): the BPF to follow. The BPF assumes values between 0 and 1 (curve done with power changes otherwise).
min-scaling (positive number): min dur (e.g, 1/16)
max-scaling (positive number): max dur
rnd-deviation (float): amount by which the resulting value for the heuristic deviates from given BPF. 0 means no deviation, 0.5 means the value may deviate up to halve the original BPF value (to either side)."
	 ()
	 (let* ((BPF-xs (pw::g-scaling (pw::g-power (ccl::pwgl-sample BPF n) 3)
					min-scaling max-scaling))
		(abs-rnd-deviation (abs rnd-deviation))
		(rnds (loop for i from 1 to n
		 	    collect (pw::g-random (* abs-rnd-deviation -1) abs-rnd-deviation)))
		(BPF-rnd-xs (pw::g+ BPF-xs (pw::g* BPF-xs rnds))))
	   (hr-rhythms-one-voice #'(lambda (xs) 
				     "Returns a heuristic -- better BPF matches are preferred. Essentially, returns the abs difference between current dur and corresponding env value."
				     (- 1000 (* (abs (- (first (last xs)) (nth (1- (length xs)) 
									       BPF-rnd-xs))) 
						100)))		       
				 voices
				 :list-with-all-durations)))


;; no-two-consecutive-syncopations

(defun no-two-consecutive-syncopations-rule (offs1 offs2)
  "For any two consecutive beats, at least one notes must start on a beat (has an offset = 0). Constraint intended for r-meter-note applied to beats with input model :offset."
  (or (= offs1 0)
      (= offs2 0)))

;; BUG: not working? 
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


;; start-with-rest

(PWGLDef start-with-rest ((rest-dur 0)
			  (voices 0)
			   &optional
			   (rule-type  () :rule-type-mbox)
			   (weight 1))
	 "Start the given voice(s) with a rest of the given duration.
Further optional arguments are inherited from r-index-rhythms-one-voice"
	 () 
	 (r-index-rhythms-one-voice #'(lambda (rhythm) (= rhythm rest-dur))
				    0
				    voices
				    :position-for-duration
				    rule-type
				    weight))


;;;
;;; Accent Model 
;;;

#|
;; Musing
Accent-rules must be applied to different score contexts. So, I would need to combine r-note-meter calls with different args (e.g., different format settings and different number of args per rule). However, it will then be difficult to combine the rating of the applied constraints. I basically need reified constraints of the cluster-engine, and likely an additional variable to store the rating. Hm...

What I could easily do is apply multiple accent rules independently. However, I think a strength of the Strasheela accent model is that it allows to combine multiple accent constraints that depend on each other.  


;; TODO:
- Decide: should different accent rules have access to different information (different format settings: :offs, :d_offs, :d_offs_m, :d_offs_m_n)? For now assume the same setting for all
- Add missing args of the Strasheela def
- Define set of accent-rules
- Write documentation
|#
(PWGLDef accent-if ((accent-rules NIL)
		    (voices 0)
		    (metric-position () (ccl::mk-menu-subview :menu-list '(":1st-beat" ":beats")))
		    (min-rating 1)
		    (strictness () (ccl::mk-menu-subview :menu-list '(":note" ":position" ":note-n-position")))
		    (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
		    &optional
		    (rule-type  () :rule-type-mbox)
		    (weight 1)
		    ;; min-rating
		    ;; strictness
		    )
  "

"
  ()
  (r-note-meter #'(lambda (d_offs1 d_offs2 d_offs3)
		    )
		voices
		:d_offs
		metric-structure
		:durations
		gracenote-mode
		rule-type weight))




;;
;; First simplified draft
;;


#|
Strasheela doc from IsLongerThanPredecessor
Note N is longer than the preceeding note and not shorter than succeeding note (duration + offsetTime used for calculating the perceived duration). If a preceeding or succeeding note does not exist (in the same temporal container) then the constraint returns 0.

If note is accented then start on a beat OR if not starts on a beat then it most accented.
|#

;; !! TODO: later allow for actual accent constraints to be given as args with fun expecting funs and returning a fun
;; TODO: 
;; - refine model: all notes beyond a certain duration are also accented -- define this with extra rule..
;; - how can I take rests into account?
;; - rule only applied when there are actually three notes in succession, i.e., not to 1st two notes. In Strasheela, rule is applied in such a way that values of 1st two "args" of coresponding function can be nil
;;
;; NOTE: constraint applied to d_offs2, but only checked after d_offs3 is bound
(defun accent-is-longer-than-predecessor-rule  (d_offs1 d_offs2 d_offs3)
  "Strait-forward but unflexible accent model implementation."
  (destructuring-bind ((dur1 offs1) (dur2 offs2) (dur3 offs3)) (list d_offs1 d_offs2 d_offs3)
    (let ((accent-rating2 (and (< dur1 dur2) (>= dur2 dur3))))
      ;; If note is accented then start on a beat (but there can be beats without accent)
      (if accent-rating2
					; (> accent-rating2 0) ;; use when generalised later...
          (= offs2 0) 
        T))))


;; TODO: 
;; - generalise to allow also a predefined accent structure in a voice with a fixed rhythm. See example TODO/accent-model-over-rhythm-voice.pwgl
(PWGLDef accent-is-longer-than-predecessor 
	 ((voices 0)
	  (metric-structure () (ccl::mk-menu-subview :menu-list '(":1st-beat" ":beats")))
	  (gracenote-mode  () (ccl::mk-menu-subview :menu-list '(":normal" ":excl-gracenotes")))
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Strait-forward but unflexible accent model implementation.
If an accent occurs, then it is on the position defined. However, random accents can happen on the 1st or last two notes.

Notes on the selected metric position (metric-structure, either :beats or :1st-beat) are rhythmically accented: such note is longer than the preceeding note and not shorter than the succeeding note.

All arguments are inherited from r-note-meter." 
	 ()
	 (r-note-meter #'accent-is-longer-than-predecessor-rule
		       voices
		       :d_offs
		       metric-structure
		       :durations
		       gracenote-mode
		       rule-type weight))



#|

;;;
;;; Patterns
;;;

;; Every 6th variable must be equal, resulting in a cycle of length 6
;; TODO: generalise with a higher-order fun (a bit challenging to dynamically create the lambda list, perhaps with a new macro?)
#'(lambda (x1 x2 x3 x4 x5 x6)
(= x1 x6)) 

|#


