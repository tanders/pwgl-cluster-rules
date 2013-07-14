(in-package :cluster-rules)

(PWGLDef voice->start-times ((voice nil) &key (rationalize? NIL))
	 "Expects a voice and returns a list of its note and rest start times. Start times of rests are notated as negative numbers. If a score or part is given instead, then the first voice is selected.

NOTE: The resulting start times depend on the tempo of the score. If a quarternote should result in a 0.25 (or 1/4) then the tempo of the score should be constant at 60.

Args:
rationalize? (Boolean): If the score has a constant tempo of 60, then start times can be output as ratios, by setting this argument to T.
"
	 ()
	 (mappend #'(lambda (c) 
		      (if (ccl::tied-p c)
			  NIL
			(list (let ((start-time
				     ;; divide by four - transform beats for dur 1 into 1/4 
				     (/ (* (ccl::start-time c)
					   (if (ccl::rest-p c) -1 1))
					4)))
				(if rationalize? 
				    (rationalize start-time)
				  start-time)))))
		  (ccl::collect-enp-objects (first (ccl::collect-enp-objects voice :voice)) 
					    :chord)))


(PWGLDef voice->durations ((voice nil) &key (rationalize? NIL))
	 "Expects a voice and returns a list of its note and rest durations. Durations of rests are notated as negative numbers. If a score or part is given instead, then the first voice is selected.

NOTE: The resulting start times depend on the tempo of the score. If a quarternote should result in a 0.25 (or 1/4) then the tempo of the score should be constant at 60.

Args:
rationalize? (Boolean): If the score has a constant tempo of 60, then durations can be output as ratios (i.e., 1/4 means a quarter note), by setting this argument to T."
	 ()
	 (let ((starts (voice->start-times voice)))
	   (mapcar #'(lambda (start dur) 
		       (if rationalize? 
			   (rationalize (* (signum start) dur))
			   (* (signum start) dur)))
		   starts
		   (pw:x->dx
		    (append (mapcar #'abs starts)
			    (let ((c (first (last (ccl::collect-enp-objects 
						   (first (ccl::collect-enp-objects voice :voice))
						   :chord)))))
			      (if (ccl::tied-p c)
				  NIL
				(list (/ (+ (ccl::start-time c) (ccl::duration c)) 4)))))))))


(PWGLDef voice->pitches ((voice nil))
	 "Expects a voice and returns a list of its note pitches. If a score or part is given instead, then the first voice is selected. In case there is a rest, NIL is returned.
"
	 ()
	 (mappend #'(lambda (c) 
		      (cond ((ccl::rest-p c) '(NIL))
			    ((ccl::tied-p c) NIL)
			    (T (list (ccl::midi (first (ccl::collect-enp-objects c :note)))))))
		  (ccl::collect-enp-objects (first (ccl::collect-enp-objects voice :voice)) 
					    :chord)))