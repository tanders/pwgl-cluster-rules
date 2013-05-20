(in-package :cluster-rules)

;; define a user menu
(ccl::add-PWGL-user-menu 
 '(:menu-component
   ("Cluster Rules"
    (("rhythm"  (rhythm-profile-hr
		 no-two-consecutive-syncopations no-syncopation only-simple-syncopations only-simple-tuplet-offs
		 accent-is-longer-than-predecessor))
     ("harmony"  (only-scale-PCs only-chord-PCs 
		  chord-PC-at-1st-tone-HACK stepwise-non-chord-tone-resolution no-voice-crossing no-voice-crossing-poly unequal-sim-PCs unequal-sim-PCs-poly))
     ("score" (score-voice2durs))
     ("utilities"  (file-in-this-directory read-lisp-file read-harmony-file))
     ))))
