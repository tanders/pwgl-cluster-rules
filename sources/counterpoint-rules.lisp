(in-package :cluster-rules)

;;; no-voice-crossing 

(PWGLDef no-voice-crossing 
	 ((voices 0)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":all" ":beat" ":1st-beat" ":1st-voice")))
	  (gracenotes?  () :gracenotes?-include-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Voices should not cross, i.e., the pitch of simultaneous note pairs in voices always fulfills pitch1 >= pitch2.

Arguments are inherited from r-pitch-pitch."
	 () 
	 (r-pitch-pitch #'(lambda (pitches)
			    (if (and (first pitches) (second pitches)) ; no rests
				(>= (first pitches) (second pitches))
			      T))
			voices
			'(0)
			input-mode
			gracenotes?
			rule-type weight))


(PWGLDef no-voice-crossing-poly 
	 ((min-voice 0)
	  (max-voice 1)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":all" ":beat" ":1st-beat" ":1st-voice")))
	  (gracenotes?  () :gracenotes?-include-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Rule no-voice-crossing applied to all consecutive voice pairs (i.e., the pitch of simultaneous note pairs always fulfills pitch1 >= pitch2).

Args: 
min-voice/max-voice: indicate the min and max voice number involved.

Other arguments are inherited from r-pitch-pitch."
	 () 
	 (mapcar #'(lambda (i) 
		     (no-voice-crossing (list i (1+ i))
					input-mode
					gracenotes?
					rule-type weight))
		 (arithm-ser min-voice 1 (1- max-voice))))

