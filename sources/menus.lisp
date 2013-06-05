(in-package :cluster-rules)

;; define a user menu
(add-PWGL-user-menu 
 '(:menu-component
   ("Cluster Rules"
    (; ("generic" ())
     ("profile" (follow-profile-hr
		 rhythm-profile-BPF-hr ; substituted by follow-profile-hr, can be removed?
		 compose-functions)
		 ("mappings" (mp-add-offset mp-multiply mp-add-random-offset))
		 ("transformations" (trfm-scale trfm-add-BPF trfm-multiply-BPF trfm-reverse)))
     ("rhythm"  (no-two-consecutive-syncopations no-syncopation only-simple-syncopations only-simple-tuplet-offs
		 start-with-rest
		 accent-is-longer-than-predecessor))
     ("melody"  (no-repetition min/max-interval durations-control-intervals))
     ("harmony"  (only-scale-PCs only-chord-PCs chord-tone-before/after-rest
		  chord-PC-at-1st-tone-HACK stepwise-non-chord-tone-resolution unequal-sim-PCs unequal-sim-PCs-poly
		  no-empty-sim-consonances-2parts
		  tintinnabuli-M-voice tintinnabuli-T-voice))
     ("counterpoint"  (no-voice-crossing no-voice-crossing-poly))
     ("score" (voice->durations voice->pitches))
     ("utilities"  (file-in-this-directory read-lisp-file read-harmony-file))
     ))))

