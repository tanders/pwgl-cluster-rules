(in-package :cluster-rules)



;; This is the starting point for defining heuristic rules that vary somehow an existing score
(PWGLDef score-voice2durs ((score nil) (voice-no 0))
	 "Returns the list of note durations from the voice with the given voice-no.

TODO:
- integrate (ccl::tied-p chord)

Current limitations:
- Ties are not recognised and ignored.
- Note durations extracted from floats, so the precision is limited."
	 ()
	 (pw:x->dx
	  (mapcar #'(lambda (chord) 
		      (* (ccl::start-time chord)
			 (if (ccl::rest-p chord) -1 1)))
		  (ccl::collect-enp-objects
		   (nth voice-no (ccl::collect-enp-objects score :voice))
		   :chord))))

