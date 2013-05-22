(in-package :cluster-rules)

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


