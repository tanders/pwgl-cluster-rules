(in-package :cluster-rules)

;; define a user menu
(add-PWGL-user-menu 
 '(:menu-component
   ("Cluster Rules"
    (; ("generic" ())
     ("profile" (follow-timed-profile-hr
		 follow-profile-hr
		 rhythm-profile-BPF-hr 
		 compose-functions)
		 ("mappings" (mp-add-offset mp-multiply mp-add-random-offset))
		 ("transformations" (trfm-scale trfm-add-BPF trfm-multiply-BPF trfm-reverse)))
     ("rhythm"  ("Cluster Engine" (cluster-engine::r-metric-hierarchy
				   cluster-engine::r-rhythm-hierarchy))
                (no-two-consecutive-syncopations no-syncopation only-simple-syncopations only-simple-tuplet-offs
		 start-with-rest
		 metric-offset-of-motif phrase-length
		 similar-sim-durations
		 ; accent-is-longer-than-predecessor
		 metric-accents
		 accents-in-other-voice)
                 ("accent rules" (mk-accent-has-at-least-duration-ar)))
     ("melody"  ("Cluster Engine" (cluster-engine::r-mel-interval-one-voice))
                (min/max-interval set-pitches set-intervals accumulative-interval
		 no-direct-repetition no-repetition 
		 restrict-consecutive-directions resolve-skips
		 durations-control-intervals
		 ))
     ("harmony"  ("Cluster Engine" (cluster-engine::r-chords))
                 (only-scale-PCs only-chord-PCs chord-tone-before/after-rest
		  chord-PC-at-1st-tone-HACK stepwise-non-chord-tone-resolution 
		  unequal-sim-PCs number-of-sim-PCs set-harmonic-intervals
		  tintinnabuli-M-voice tintinnabuli-T-voice))
     ("counterpoint" ("Cluster Engine" (cluster-engine::r-canon))
                     (no-voice-crossing no-parallels))
     ("score" (voice->start-times voice->durations voice->pitches
	       set-staff-clefs
	       set-staff-instruments))
     ("utilities"  (file-in-this-directory read-lisp-file read-harmony-file 
		    output-filename))
     ))))

