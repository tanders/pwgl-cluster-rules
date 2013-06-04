(in-package :cluster-rules)


;; pitch-profile-hr

;; TODO:
;; - support also BPFs
;; - turn into polyphonic version, with different pitches per voice
;; - copy this def for rhythms... -- turn lambda function into name function, and then reuse that?
;;
(PWGLDef pitch-profile-hr  
	 ((voices 0)
	  (n 0)
	  (profile NIL) ; for now list of ints
	  (weight-offset 0)
	  &key
	  (transform NIL)
	  (map NIL)
	  ;; TODO: implement support for this arg 
	  ;; TODO: test whether I can have a menu as input (for predefined transformations) but nevertheless also give it a function as arg 
	  (constrain () (ccl::mk-menu-subview :menu-list '(":profile" ":intervals" ":directions"))))
	 "Heuristic rule. The resulting pitches follow input profile (numbers, a voice, or BPFs).

Args:

voices (int or list of ints): The voice(s) to which the constraint is applied. 

n (int): The first n notes are affected (if n is greater than the length of profile, then that length is taken). If 0, then n is disregarded.

profile: Specifies the profile, which should be followed. This can be either a list of numbers (ints, floats or ratios), a voice object, a [BPF], or a list of any of these (if multiple voices should be constrained). In case a full score object is given, the first voice is extracted. If the voice object contains chords, then only the first chord note is extracted.

weight-offset (int): offset to the heuristic weight of this rule (the higher the offset, the more important this rule becomes compared with other heuristic rules).

Key args:
map: Change every individual input value with some definition (e.g., add some random offset). Expects a function or abstraction expecting a number and returning a number. 
transform: Change the whole sequence of input profile with some definition (e.g., reverse or rotate the sequence, or remove some elements). Expects a function or abstraction expecting a list and returning a list.
constrain: Select whether pitches should follow the profile directly, or whether pitch intervals should follow the intervals between profile, or pitch directions should follow pitch directions."
	 ()
	 (flet ((direction-int (x1 x2)
		 "Encodes the direction of the interval between x1 and x2 as integer. An interval 'upwards' (the predecessor is smaller than the successor) is represented by 2, an 'horizontal' interval (the predecessor and the successor are equal) is represented by 1, and an interval 'downwards' by 0."
	     (cond ((< x1 x2) 2)
		   ((= x1 x2) 1)
		   ((> x1 x2) 0))))
	   (let* ((plain-profile (cond ((listp profile) profile) ;; TODO: revise for polyphonic case
				       ((or (ccl::voice-p profile) (ccl::score-p profile) (ccl::part-p profile)) 
					(mapcar #'(lambda (chord) 
						    (ccl::midi (first (ccl::collect-enp-objects chord :note))))
						(ccl::collect-enp-objects (first (ccl::collect-enp-objects profile :voice)) 
									  :chord)))
				       (T (error "Neither list nor score nor BPF: ~A" profile))))
		  (transformed-profile (if transform 
					   (funcall transform plain-profile)
					 plain-profile))
		  (mapped-profile (if map 
				      (mapcar map transformed-profile)
				    transformed-profile)))
	     (hr-pitches-one-voice #'(lambda (xs) 
				       "Defines a heuristic -- larger return profile are preferred. Essentially, returns the abs difference between current value and pitch."
				       (let ((l (length xs)))
					 (if (and (or (= n 0) (<= l n))
						  (<= l (length mapped-profile)))
					     (+ (- (abs
						    (case constrain
						      (:profile  (- (first (last xs)) (nth (1- l) mapped-profile)))
						      ;; distance between distances of last two vals
						      (:intervals (if (>= l 2)
						    		      (- (apply #'- (last xs 2))
						    			 (- (nth (- l 2) mapped-profile)
						    			    (nth (- l 1) mapped-profile)))
						    		    0))
						      ;; distance between directions of last two vals
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
				   :all-pitches))))


;; no-repetition

(PWGLDef no-repetition ((voice 0))
	 "Disallows any direct pitch repetition. 

Args: 
voice: the number of the voice to constrain."
	 () 
	 (r-pitches-one-voice #'(lambda (p1 p2) 
				  (if (and p1 p2) ; no rests
				      (/= p1 p2)
				    T))
			      voice
			      :pitches))

;; min/max-melodic-interval

(PWGLDef min/max-interval ((voice 0)
			   &key
			   (min-interval NIL)
			   (max-interval NIL))
	 "Limit the minimum/maximum melodic interval between voices at the given max-interval.

Args: 
voice: the number of the voice to constrain.
min-interval (key arg): minimum interval in semitones. Ignored if NIL. Implicitly disallows repetition if >= 1.
max-interval (key arg): maximum interval in semitones. Ignored if NIL."
	 () 
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
			      voice
			      :pitches))


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


