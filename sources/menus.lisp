(in-package :cluster-rules)

;; define a user menu
(add-PWGL-user-menu 
 '(:menu-component
   ("Cluster Rules"
    (; ("generic" ())
     ("rhythm"  (rhythm-profile-BPF-hr ; substituted by follow-values
		 no-two-consecutive-syncopations no-syncopation only-simple-syncopations only-simple-tuplet-offs
		 start-with-rest
		 accent-is-longer-than-predecessor))
     ("melody"  (pitch-profile-hr
		 no-repetition min/max-interval durations-control-intervals))
     ("harmony"  (only-scale-PCs only-chord-PCs chord-tone-before/after-rest
		  chord-PC-at-1st-tone-HACK stepwise-non-chord-tone-resolution unequal-sim-PCs unequal-sim-PCs-poly
		  no-empty-sim-consonances-2parts
		  tintinnabuli-M-voice tintinnabuli-T-voice))
     ("counterpoint"  (no-voice-crossing no-voice-crossing-poly))
     ("score" (score-voice2durs))
     ("utilities"  (file-in-this-directory read-lisp-file read-harmony-file))
     ))))
