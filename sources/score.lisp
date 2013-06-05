(in-package :cluster-rules)



;; This is the starting point for defining heuristic rules that vary somehow an existing score
;;
;; NOTE: results also depend on tempo!
(PWGLDef voice->durations ((score nil))
	 "Expects a voice and returns a list of its note durations. If a score or part is given instead, then the first voice is selected.

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
		  (ccl::collect-enp-objects (first (ccl::collect-enp-objects score :voice)) 
					    :chord))))


(PWGLDef voice->pitches ((score nil))
	 "Expects a voice and returns a list of its note pitches. If a score or part is given instead, then the first voice is selected."
	 ()
	 (mapcar #'(lambda (chord) 
		     (ccl::midi (first (ccl::collect-enp-objects chord :note))))
		 (ccl::collect-enp-objects (first (ccl::collect-enp-objects score :voice)) 
					   :chord)))