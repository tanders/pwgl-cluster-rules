(in-package :cluster-rules)

;;;
;;; General defs
;;;

(ccl::add-box-type :gracenotes?-include-mbox 
		   ;; TODO: this is probably an unnecessary nesting of ccl::mk-menu-subview
		   `(ccl::mk-menu-subview :menu-list ,(ccl::mk-menu-subview :menu-list '(":include-gracenotes" ":exclude-gracenotes")) :value 1))

(ccl::add-box-type :gracenotes?-exclude-mbox 
		   ;; TODO: this is probably an unnecessary nesting of ccl::mk-menu-subview
		   `(ccl::mk-menu-subview :menu-list ,(ccl::mk-menu-subview :menu-list '(":include-gracenotes" ":exclude-gracenotes")) :value 1))



;;;
;;; Follow harmony rules 
;;;

(defun in-harmony? (pitches)
  "The PC of (first pitches) is in the PCs of (second pitches)."
  (if (first pitches)
      (member (mod (first pitches) 12)
	      (mapcar #'(lambda (p) (mod p 12))
		      (second pitches)))
    t))

;;; only-scale-PCs 

(PWGLDef only-scale-PCs 
	 ((voice 2)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":all" ":beat" ":1st-beat")))
	  (gracenotes?  () :gracenotes?-include-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1)
	  (scale-voice 0))
	 "Tones (PC) in the given voice must be a member of the underlying scale (its PCs). The scale is represented as a simultaneous chord in another voice (voice 0 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
voice: the voice to which this constraint is applied.
scale-voice (optional): the voice representing the underlying scale.

Other arguments are inherited from r-pitch-pitch."
	 () 
	 (r-pitch-pitch #'in-harmony?
			(list voice scale-voice)
			'(0)
			input-mode
			gracenotes?
			rule-type weight))


;; only-chord-PCs

(PWGLDef only-chord-PCs 
	 ((voice 2)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":beat" ":all" ":1st-beat")))
	  (gracenotes?  () :gracenotes?-exclude-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1)
	  (chord-voice 1))
	 "Tones (PC) in the given voice must be a member of the underlying chord (its PCs). The chord is represented as a simultaneous chord in another voice (voice 1 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
voice: the voice to which this constraint is applied.
scale-voice (optional): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat."
	 () 
	 (r-pitch-pitch #'in-harmony?
			(list voice chord-voice)
			'(0)
			input-mode
			gracenotes?
			rule-type weight))


;;;
;;; Other pitch related rules
;;;

;; chord-PC-at-1st-tone

;; NOTE: hack
(PWGLDef chord-PC-at-1st-tone-HACK 
	 ((chords ()) 
	  (voice 2)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":position-for-pitches" ":index-for-cell")))
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1)
	  )
	 "HACK: The very first tone (PC) in given voice number must be a member of the first of the given chords (the list of list of chords from read-harmony-file).

NOTE: an index variant for a pitch-pitch constraint (which could access the sim chord PCs) is not available, therefore this workaround."
	 () 
	 (r-index-pitches-one-voice #'(lambda (pitch) 
					(member (mod pitch 12)
						(mapcar #'(lambda (chord-pitch) (mod chord-pitch 12)) 
							(first chords))))
				    '(0) ; positions
				    voice
				    input-mode
				    rule-type weight))


;; stepwise-non-chord-tone-resolution

(PWGLDef stepwise-non-chord-tone-resolution 
	 ((step-size 2) 
	  (voice 2)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":all" ":beat" ":1st-beat")))
	  (gracenotes?  () :gracenotes?-exclude-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1)
	  (chord-voice 1))
	 "Every tone (PC) that is not a chord tone (member of sim chord PCs, in voice 1 by default) is reached/left by a step of the given step size.

Args: 
voice: the voice to which this constraint is applied.
scale-voice (optional): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
	 () 
	 (r-pitch-pitch #'(lambda (pitches1 pitches2 pitches3)
			    ;; Every pitchesN is a list of the form (chord-pitches voice-pitch) 
			    (let ((voice-pitch2 (second pitches2)))
			      (if (not (member (mod voice-pitch2 12) ; middle PC
					       ;; chord PCs
					       (mapcar #'(lambda (p) (mod p 12)) (first pitches2))))
				  (and (<= (abs (- (second pitches1) voice-pitch2)) step-size)
				       (<= (abs (- voice-pitch2 (second pitches3))) step-size))
				t)))
			(list chord-voice voice)
			'(0)
			input-mode
			gracenotes?
			rule-type weight))


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
			    (>= (first pitches) (second pitches)))
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


;;; unequal-sim-PCs

(PWGLDef unequal-sim-PCs 
	 ((voices 0)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":all" ":beat" ":1st-beat" ":1st-voice")))
	  (gracenotes?  () :gracenotes?-include-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "[Quasi aux def] The PCs of the 1st given voice are unequal to the sim PCs of the remaining voices."
	 () 
	 (r-pitch-pitch #'(lambda (pitches)
			    (not (member (mod (first pitches) 12) 
					 (mapcar #'(lambda (p) (mod p 12)) 
						 (rest pitches)))))
			voices
			'(0)
			input-mode
			gracenotes?
			rule-type weight))

(PWGLDef unequal-sim-PCs-poly
	 ((voices 0)
	  (input-mode  () (ccl::mk-menu-subview :menu-list '(":all" ":beat" ":1st-beat" ":1st-voice")))
	  (gracenotes?  () :gracenotes?-include-mbox)
	  &optional
	  (rule-type  () :rule-type-mbox)
	  (weight 1))
	 "Sim PCs in all given voices (ints in ascending order) are unequal to each other..

Arguments are inherited from r-pitch-pitch."
	 () 
	 (let ((len (length voices))
	       (rev (reverse voices)))
	   (mapcar #'(lambda (i) 
		       (unequal-sim-PCs (subseq rev i len)
					input-mode
					gracenotes?
					rule-type weight))
		   (arithm-ser 0 1 (1- len)))))




