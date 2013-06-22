(in-package :cluster-rules)

;;; no-voice-crossing 

(PWGLDef no-voice-crossing 
	 ((voices 0)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":all" ":beat" ":1st-beat" ":1st-voice")))
	  (gracenotes?  () :gracenotes?-include-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Voices should not cross, i.e., the pitch of simultaneous note pairs in voices are always sorted in decreasing order.

Arguments are inherited from r-pitch-pitch."
	 () 
	 (r-pitch-pitch #'(lambda (pitches)
			    (apply #'>= (remove NIL pitches))) ; no rests -- no NILs			     
			voices
			'(0)
			input-mode
			gracenotes?
			rule-type weight))
