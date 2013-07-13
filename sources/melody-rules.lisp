(in-package :cluster-rules)


;; pitch-profile-hr

;; TODO:
;; - !! Efficiency: change mapped-profile from list into array/vector for faster access during search -- see PWConstraints example.. 
;; - generalise this def for rhythms -- work in progress.
;; - ? Add support for "simple score format" by Kilian -- I can more easily transform such scores before handing them over 
;;   -> postpone until I actually need it
;; - turn into polyphonic version, with different pitches per voice
;;   -> postpone until I actually need it
;; - ? Some case of arg constrain that is more flexible than :intervals but less than :directions -- allow for +/- one semitone (is this really worth it -- after all, these are just heuristics? It could be if it is combined with other heuristic constraints)
;;
;; - OK change name to follow-profile-hr, and make sure you change all example patches. Store elsewhere in menu and file?
;; - OK support also BPFs
;; - OK Predefine some functions for transform and map
;; - OK How to handle rests in a given score? The current code simply ignores them, only returns notes of pitches. However, if rest are included then the pitch profile is still folled by the notes, so it appears I only need to deal with rests in the rhythmic variant of this definition.
(PWGLDef follow-profile-hr  
	 ((voices 0)
	  (n 0)
	  (profile NIL) 
	  (mode () (ccl::mk-menu-subview :menu-list '(":pitch" ":rhythm")))
	  ;; TODO: test whether I can have a menu as input (for predefined transformations) but nevertheless also give it a function as arg 
	  (constrain () (ccl::mk-menu-subview :menu-list '(":profile" ":intervals" ":directions")))
	  &key
	  (weight-offset 0)
	  (transform NIL)
	  (map NIL))
	 "Heuristic rule. The pitches or rhythmic values of the resulting music follow the given profile (numbers, a voice, or BPFs).

Note: If this rule is used with pitch/rhythm motifs, then only the selection of the 1st motif note is controlled by the rule (in future it would be nice to control the average pitch/rhythm of motifs, but that would require different rule applicators).

Args:

voices (int or list of ints): The voice(s) to which the constraint is applied. 

n (int): The first n notes are affected (if n is greater than the length of profile, then that length is taken). If 0, then n is disregarded. NOTE: if a BPF is used then make sure n is greater than 0.

profile: Specifies the profile, which should be followed. This can be either a list of numbers (ints, floats or ratios), a voice object (or a score/part), a BPF object, or a list of any of these (if multiple voices should be constrained). In case a score or part object is given, then only the first voice is extracted and used. If voice objects contains chords, then only the first chord note is extracted. In case a BPF is given, then that BPF is sampled (n samples) and the y values are used.

mode: Select whether to constrain either the rhythmic values (rhythm) or the pitches (pitch). If you want to constrain both, then simply use two instances of this rule with different mode settings.

constrain: Select whether pitch/rhythm should follow the profile directly, or whether pitch/rhythm intervals should follow the intervals between profile, or pitch/rhythm directions should follow the directions of profile intervals.

Key args:

weight-offset (int): offset to the heuristic weight of this rule (the higher the offset, the more important this rule becomes compared with other heuristic rules).

transform: Change the whole sequence of input profile with some definition (e.g., reverse or rotate the sequence, or remove some elements). Expects a function or abstraction expecting a list and returning a list. Some functions are already predefined for convenience (menu Cluster Rules - profile - transformations).

map: Tranform every individual profile value with some definition (e.g., add some random offset). Expects a function or abstraction expecting a number and returning a number. Some functions are already predefined for convenience (menu Cluster Rules - profile - mappings). You can used multiple functions together (transform the transformation of profile values...) by combining multiple functions with compose-functions.
"
	 (:groupings '(3 2))
	 (flet ((direction-int (x1 x2)
		 "Encodes the direction of the interval between x1 and x2 as integer. An interval 'upwards' (the predecessor is smaller than the successor) is represented by 2, an 'horizontal' interval (the predecessor and the successor are equal) is represented by 1, and an interval 'downwards' by 0."
	     (cond ((< x1 x2) 2)
		   ((= x1 x2) 1)
		   ((> x1 x2) 0))))
	   (let* ((plain-profile 
		   ;; process different inputs: list of int, BPF, score... 
		   (cond ((listp profile) profile) ;; TODO: revise for polyphonic case (test for list of numbers)
			 ((ccl::break-point-function-p profile)
			  (if (> n 0)  
			      (ccl::pwgl-sample profile n)
			    (progn (warn "Cannot sample BPF with n set to 0") NIL)))
			 ((or (ccl::voice-p profile) (ccl::score-p profile) (ccl::part-p profile)) 
			  (case mode
			    (:pitch (voice->pitches profile))
			    (:rhythm (voice->durations profile))))
			 (T (error "Neither list nor score nor BPF: ~A" profile))))
		  (transformed-profile (if transform 
					   (funcall transform plain-profile)
					 plain-profile))
		  (mapped-profile (if map 
				      (mapcar map transformed-profile)
				    transformed-profile)))
	     (funcall (case mode
			(:pitch #'hr-pitches-one-voice)
			(:rhythm #'hr-rhythms-one-voice))
		      #'(lambda (xs) 
			  "Defines a heuristic -- larger return profile are preferred. Essentially, returns the abs difference between current value and pitch."
			  (let ((l (length xs)))
			    (if (and (or (= n 0) (<= l n))
				     (<= l (length mapped-profile)))
				(+ (- (abs
				       ;; process different settings for constrain
				       (case constrain
					 (:profile  (- (first (last xs)) (nth (1- l) mapped-profile)))
					 ;; distance between distances of last two vals
					 ;; TODO: unfinished for rhythm -- how to compute distance of distances?
					 (:intervals (if (>= l 2)
							 (case mode
							   (:pitch (- (apply #'- (last xs 2))
								      (- (nth (- l 2) mapped-profile)
									 (nth (- l 1) mapped-profile))))
							   ;; for rhythm distance is quotient not difference
							   (:rhythm (- (apply #'/ (last xs 2))
								       (/ (nth (- l 2) mapped-profile)
									  (nth (- l 1) mapped-profile)))))
						       0))
					 ;; distance between directions of last two vals
					 ;; TODO: for rhythm def direction as whether distances are smaller or larger than 1 not 0
					 (:directions (if (>= l 2)
							  (- (apply #'direction-int (last xs 2))
							     (direction-int
							      (nth (- l 2) mapped-profile)
							      (nth (- l 1) mapped-profile)))
							0)))))
				   weight-offset)
			      ;; otherwise no preference
			      0)))
		      voices
		      (case mode
			(:pitch :all-pitches)
			(:rhythm :list-with-all-durations))))))


;; Some mapping functions for pitch-profile-hr or rhythm-profile-hr

(defun compose-functions (&rest funs)
  "Expects any number of mapping or transformation functions and composes these into a single function that can be given to pitch-profile-hr or rhythm-profile-hr. The functions will be called in their given order."
  #'(lambda (x) 
      (reduce #'(lambda (y f) (funcall f y)) funs :initial-value x)))

(defun mp-add-offset (offset)
  "Returns a mapping function for pitch-profile-hr or rhythm-profile-hr. offset is added to the original value."
  #'(lambda (x) (+ x offset)))

(defun mp-multiply (factor)
  "Returns a mapping function for pitch-profile-hr or rhythm-profile-hr. factor is multiplied to the original value."
  #'(lambda (x) (* x factor)))

(defun mp-add-random-offset (max-random-offset)
  "Returns a mapping function for pitch-profile-hr or rhythm-profile-hr. max-random-offset is the maximun random deviation, above or below the original value."
  #'(lambda (x) 
      (let ((abs-rnd-offset (abs max-random-offset)))
	(+ x (pw::g-random (* abs-rnd-offset -1) abs-rnd-offset)))))


;; Some transformation functions for pitch-profile-hr or rhythm-profile-hr

(defun trfm-scale (min max)
  "Returns a transformation function for pitch-profile-hr or rhythm-profile-hr. The original values are scaled between min and max."
  #'(lambda (xs) (pw::g-scaling xs min max)))

(defun trfm-add-BPF (BPF)
  "Returns a transformation function for pitch-profile-hr or rhythm-profile-hr. To each original value the corresponding BPF vcalue is added."
  #'(lambda (xs) 
      (mapcar #'+ xs (ccl::pwgl-sample BPF (length xs)))))

(defun trfm-multiply-BPF (BPF)
  "Returns a transformation function for pitch-profile-hr or rhythm-profile-hr. To each original value the corresponding BPF vcalue is multiplied."
  #'(lambda (xs) 
      (mapcar #'* xs (ccl::pwgl-sample BPF (length xs)))))

(defun trfm-reverse (min max)
  "Returns a transformation function for pitch-profile-hr or rhythm-profile-hr. The original value sequence is reversed."
  #'(lambda (xs) (reverse xs)))



;; no-direct-repetition

(PWGLDef no-direct-repetition ((voices 0)			
			       &optional
			       (rule-type  () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
			       (weight 1))
	 "Disallows any direct pitch repetition. 

Args: 
voices: the number of the voice(s) to constrain.

Optional arguments are inherited from r-pitches-one-voice."
	 () 
	 (r-pitches-one-voice #'(lambda (p1 p2) 
				  (if (and p1 p2) ; no rests
				      (/= p1 p2)
				    T))
			      voices
			      :pitches
			      rule-type weight))


;; no-repetition

(PWGLDef no-repetition ((voices 0)
			(window 2)
			(mode () (ccl::mk-menu-subview :menu-list '(":pitches" ":pcs")))			
			&optional
			(rule-type  () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
			(weight 1))
	 "Disallows pitch repetitions within a window of a give number of notes.

Args: 
voices: the number of the voice(s) to constrain.
window: the number of notes among which no repetition should happen (if this is larger than the currently available number, then simply the available notes are taken).
mode: whether to disallow repeated pitches (:pitches) or pitch classes (:pcs).

Optional arguments are inherited from r-pitches-one-voice."
	 () 
	 (r-pitches-one-voice #'(lambda (pitches) 
				  (let* ((ps (mapcar #'(lambda (p)
							 (case mode 
							   (:pitches p)
							   (:pcs (mod p 12))))
						     (last pitches window)))
					 (p1 (first (last ps))))
				    (if p1 ; no rest
					(not (member p1 (butlast ps)))
					;; (progn (format t "no-repetition -- p: ~A, ps: ~A, result: ~A ~%"
					;; 	       p1 (butlast ps) (not (member p1 (butlast ps))))
					;;        (not (member p1 (butlast ps))))
				      T)))
			      voices
			      :all-pitches
			      rule-type weight))


;; min/max-melodic-interval

;; TODO: 
;; - !! Efficiency: use array/vector instead of list 
(PWGLDef min/max-interval ((voices 0)
			   &key
			   (min-interval NIL)
			   (max-interval NIL)
			   (n 0)
			   (rule-type  () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
			   (weight 1))
	 "Limit the minimum/maximum melodic interval for the given voice.

Args: 
voices (int or list of ints): the number of the voice(s) to constrain.

key-args:
min-interval (number, BPF or NIL): minimum interval in semitones. Ignored if NIL. Implicitly disallows repetition if >= 1. If a BPF, then the BPF specifies how the min interval changes over n notes (i.e., BPF specifies n-1 intervals).
max-interval (number, BPF or NIL): maximum interval in semitones. Ignored if NIL. If a BPF, then the BPF specifies how the max interval changes over n notes.
n (int): The first n notes are affected. If 0, then n is disregarded. NOTE: if any BPF is set then make sure n is greater than 0.

Args rule-type and weight inherited from r-pitches-one-voice."
	 ()
	 (if (some #'ccl::break-point-function-p (list min-interval max-interval))	   
	     ;; one or both intervals are BPF  
	     (flet ((make-intervals (min/max-interval)
			"Transforms BPF (or scalar) interval into list/array of interval numbers."	    
			(cond ((ccl::break-point-function-p min/max-interval)
			       (if (> n 0)  
				   (ccl::pwgl-sample min/max-interval (1- n))
				 (progn (warn "Cannot sample BPF with n set to 0")
					NIL)))
			      ((numberp min/max-interval) (make-list n :initial-element min/max-interval))
			      ((null min/max-interval) min/max-interval))))
	       (let ((min-intervals (make-intervals min-interval))
		     (max-intervals (make-intervals max-interval)))
		 (r-pitches-one-voice #'(lambda (pitches)					  
					  (let ((l (length pitches)))
					    (if (and (>= l 2) (<= l n))
						(let* ((last-pitches (last pitches 2))
						       (pitch1 (second last-pitches))
						       (pitch2 (first last-pitches)))
						  (if (and pitch1 pitch2) ; no rests
						      (let ((interval (abs (- pitch1 pitch2))))
							(and (if min-intervals ;; TODO: efficiency: this test only required once
								 (<= (nth (- l 2) min-intervals) interval)
							       T)
							     (if max-intervals ;; TODO: efficiency: this test only required once
								 (progn 
								   ;; (format t "min/max-interval -- interval: ~A, max-interval: ~A, result: ~A ~%"
								   ;; 	   interval 
								   ;; 	   (nth (- l 2) max-intervals)
								   ;; 	   (<= interval (nth (- l 2) max-intervals)))
								   (<= interval (nth (- l 2) max-intervals)))
							       T)))
						    T))
					      T)))
					    voices
					    :all-pitches
					    rule-type
					    weight)))
	   ;; both intervals are scalars
	   (r-pitches-one-voice #'(lambda (pitch1 pitch2)
				    (if (and pitch1 pitch2) ; no rests
					(let ((interval (abs (- pitch1 pitch2))))
					  (and (if min-interval 
						   (<= min-interval interval)
						 T)
					       (if max-interval 
						   (<= interval max-interval)
						 T)))
				      T))
				voices
				:pitches
				rule-type
				weight)))


;; set-intervals

(PWGLDef set-intervals ((intervals ())
			(absolute? () (ccl::mk-menu-subview :menu-list '(":absolute" ":up/down")))
			(mode () (ccl::mk-menu-subview :menu-list '(":only-given" ":exclude-given")))
			(voices 0)
			&optional
			(rule-type () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
			(weight 1))
	 "Restricts the melodic intervals to those intervals specified. 

Args:
  intervals (list of ints): Specified intervals.
  absolute?: Controls whether the direction of the intervals is taken into account. The direction can be specified with a sign (positive for upwards, negative for downwards). If absolute? is set to :absolute, then the specified intervals can be used for any direction. By contrast, :up/down takes the sign of the given intervals into account.
  mode: Controls whether to only use the given intervals (:only-given), or whether to only use intervals that are not given (:exclude-given).

Other arguments are inherited from R-pitches-one-voice."
	 () 
	 (R-pitches-one-voice #'(lambda (pitch1 pitch2)
				  (if (and pitch1 pitch2) ; no rests
				      (let* ((interval (- pitch2 pitch1))
					     (member? (member (case absolute?
								(:up/down interval)
								(:absolute (abs interval)))
							      (case absolute?
								(:up/down intervals)
								(:absolute (mapcar #'abs intervals))))))
					(case mode
					  (:only-given member?)
					  (:exclude-given (not member?))))
				    T))
	   voices :pitches rule-type weight))


;; accumulative-interval

;; TODO:
;; - Rule for first n-1 notes ignored -- OK?
;; - Test rule further -- do settings work and make sense?
;; - Improve efficiency: Replace interval BPF list with vector
(PWGLDef accumulative-interval ((voices 0)
				(n 2)
				(interval 7)
				(condition () (ccl::mk-menu-subview :menu-list '(":max" ":min" ":equal")))
				&optional
				(sublists () (ccl::mk-menu-subview :menu-list '(":constrain" ":ignore")))
				(number-of-notes 0)
				(rule-type () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
				(weight 1))
	 "The sum of melodic intervals between the pitches of n notes is smaller / greater than the given interval. If there are any rests among the last n notes then this rule is ignored.

Args:
  n (int): number of notes involved
  interval (int or BPF): the max/min interval. If a BPF, then it defines how the interval changes across the voice(s). However, in that case the arg number-of-notes must be given, otherwise the rule throws an error.
  condition: the relation that should hold between the sum of intervals and the given interval: whether the sum should not exceed (:max) or be exactly (:equal) or be at least (:min) the given interval. 

Optional args:
  sublists: whether or not to also constrain the intervals between the last n-1, n-2 ... notes in the same way. This argument is only effective if condition is set to :max, otherwise it setting is always :ignore.
  number-of-notes: the number of variables set in clusterengine. This argument is required if interval is a BPF.   

Other arguments are inherited from R-pitches-one-voice."
	 ()
	 (let* ((BPF? (ccl::break-point-function-p interval))
		(intervals (if (and BPF? (> number-of-notes 0))				
			       (ccl::pwgl-sample interval number-of-notes)
			     (when BPF?
			       (error "accumulative-interval: Cannot sample BPF with the keyword argument number-of-notes set to its default 0.")))))
	   (flet ((rule-application (n)
		    (R-pitches-one-voice #'(lambda (pitches)
					     (let ((n-pitches (last pitches n)))
					       (if (and (= (length n-pitches) n)
							(every #'identity pitches)) ; no rests
						   (let ((interv (if BPF?
								     (nth (1- (length pitches)) intervals)
								   interval)))
						     (funcall (case condition
								(:max #'<=)
								(:min #'>=)
								(:equal #'=))					 
							      (abs (- (first (last pitches)) (first pitches)))
							      interv))
						 T)))
					 voices :all-pitches rule-type weight))) 
	     (if (and (eql sublists :constrain)
		      (eql condition :max))
		 (mappend #'rule-application
			  (arithm-ser 2 1 n))
	       (rule-application n)))))



;; durations-control-intervals

(PWGLDef durations-control-intervals ((rel-factor 32)
				      (acc-factor 2)
				      &optional
				      (rule-type  () :rule-type-mbox)
				      (weight 1))
	 "Pitch intervals and durations are lineary related.

Args:
rel-factor (relation factor): the interval is about the duration times rel-factor.
acc-factor (accuracy factor): factor how much the interval can deviate from that relation above and below. 

Examples: If rel-factor is 1 and acc-factor is also one, then the duration of a note would need to be the same as the interval starting at it (e.g., duration = 2 and interval is 2). If rel-factor is 32 and acc-factor is 2 (the defaults) the the interval can be any value between duration*32/2 and duration*32*2.  

Optional arguments are inherited from r-rhythm-pitch-one-voice."
	 () 
	 (r-rhythm-pitch-one-voice #'(lambda (dur-pitch1 dur-pitch2)
				       (if (and (second dur-pitch1) (second dur-pitch2)) ; no rests
					   (let ((dur1 (first dur-pitch1))
						 (interval (abs (- (second dur-pitch1) (second dur-pitch2)))))
					     (<= (/ interval acc-factor)
						 (* dur1 rel-factor)
						 (* interval acc-factor)))
					 T))
					 ;; old version
					 ;; (cond ((>= dur1 1/2) (<= interval 16))
					 ;;       ((>= dur1 1/6) (<= interval 5))
					 ;;       ((<= dur1 1/16) (<= interval 2)))
				   voices :rhythm/pitch :exclude-gracenotes rule-type weight))


;; restrict-consecutive-directions

;; TODO: 
;; - allow n to be controlled by a BPF
(PWGLDef restrict-consecutive-directions ((n 4)
					  (direction () (ccl::mk-menu-subview :menu-list 
							 '(":either" ":either-strict"
							   ":ascending" ":ascending-strict"
							   ":descending" ":descending-strict")))
					  ;; (condition () (ccl::mk-menu-subview :menu-list '(":at-most" ":exactly" ":at-least")))
					  ;; "condition: Which restriction to apply? If :at-most, then at most n notes can be connected by intervals of the same given direction, and so forth."
					  (voices 0)
					  &optional
					  (rule-type () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
					  (weight 1))
	 "This rule controls how many consecutive intervals can be ascending or descending. At most n notes can be connected by intervals of the same direction.

Args:
  n (int): How many consecutive notes are taken into account?  
  direction: Which interval direction is taken into account? For example, :ascending means that the rule only looks at consecutive ascending intervals. Without the ending -strict, note repetitions are also counted as the same direction. 

In case of intermitting rests the rule is not applied.

Other arguments are inherited from R-pitches-one-voice."
	 () 
	 (R-pitches-one-voice #'(lambda (all-pitches)
				  (let ((pitches (last all-pitches (1+ n))))
				    (if (and (= (length pitches) (1+ n))
					     (every #'(lambda (p) (not (null p))) pitches)) ; no rests
					(progn 
					  ;; (format t "restrict-consecutive-directions: all-pitches: ~A, ~%   pitches: ~A~%" 
					  ;; 	  all-pitches pitches)
					  (not 
					   (case direction
					     (:ascending (apply #'<= pitches))
					     (:ascending-strict (apply #'< pitches))
					     (:descending (apply #'>= pitches))
					     (:descending-strict (apply #'> pitches))
					     (:either (or (apply #'<= pitches)
							  (apply #'>= pitches)))
					     (:either-strict (or (apply #'< pitches)
								 (apply #'> pitches))))))
				      T)))
	  voices :all-pitches rule-type weight))


;; resolve-skips

;; To-do: 
;; - ?? allow n to be controlled by a BPF
(PWGLDef resolve-skips ((skip-size 6)
			(resolution-size 2)
			(repetition? () (ccl::mk-menu-subview :menu-list '(":disallow" ":allow")))
			(voices 0)
			&optional
			(rule-type () (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
			(weight 1))
	 "Resolve any skip larger than skip-size by an interval in the opposite direction.

Args:
  skip-size: The minimum interval size (in semitones) that triggers this rule.
  resolution-size: The maximum interval size that is allowed as a resolution.
  repetition?: Whether or not tone repetitions are allowed as resolution.

Other arguments are inherited from R-pitches-one-voice."
	 () 
	 (R-pitches-one-voice #'(lambda (pitch1 pitch2 pitch3)
				  (if (and pitch1 pitch2 pitch3) ; no rests
				      (let ((int1 (- pitch2 pitch1)) 
					    (int2 (- pitch3 pitch2)))
					(if (>= (abs int1) skip-size)
					  (and (<= (abs int2) resolution-size)
					       (/= (signum int1) (signum int2))
					       (case repetition?
						 (:disallow (> (abs int2) 0))
						 (:allow T)))
					  T))
				    T))
	  voices :pitches rule-type weight))
