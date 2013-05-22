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
;;; Aux harmony defs
;;;

;; ;; currently unused
;; (defun PC-member (pitch1 pitches2)
;;   "Returns T if the PC of pitch1 is a member in the PC set of pitches2."
;;   (member (mod pitch1 12) 
;;           (mapcar #'(lambda (p) (mod p 12)) pitches2)))

; (defun PC-member-save (pitch1 pitches2)
;  "Returns T if the PC of pitch1 is a member in the PC set of pitches2. Rests (i.e. pitches that are nil) are taken care of (e.g., if pitch1 is nil then PC-member returns T."
;  (if pitch1
;      (member (mod pitch1 12) 
;              (mapcar #'(lambda (p) (mod p 12))
;                      (remove NIL pitches2)))
;    T))


;;;
;;; Follow harmony rules 
;;;

(defun in-harmony? (pitches)
  "The PC of (first pitches) is in the PCs of (second pitches)."
  (if (and (first pitches) (second pitches)) ;; no rests
      (member (mod (first pitches) 12)
	      (mapcar #'(lambda (p) (mod p 12))
		      (second pitches)))
    T))

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
scale-voice (optional, default 0): the voice representing the underlying scale.

Other arguments are inherited from r-pitch-pitch."
	 () 
	 (r-pitch-pitch #'in-harmony?
			(list voice scale-voice)
			'(0)
			input-mode
			gracenotes?
			rule-type weight))


;; only-chord-PCs

(PWGLDef only-chord-PCs ((voice 2)
			 (input-mode  () (ccl::mk-menu-subview :menu-list '(":beat" ":all" ":1st-beat")))
			 (gracenotes?  () :gracenotes?-exclude-mbox)
			 &optional
			 (rule-type  () :rule-type-mbox)
			 (weight 1)
			 (chord-voice 1))
	 "Tones (PC) in the given voice must be a member of the underlying chord (its PCs). The chord is represented as a simultaneous chord in another voice (voice 1 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
voice: the voice to which this constraint is applied.
chord-voice (optional, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat."
	 () 
	 (r-pitch-pitch #'in-harmony?
			(list voice chord-voice)
			'(0)
			input-mode
			gracenotes?
			rule-type weight))


;; chord-tone-before/after-rest

(PWGLDef chord-tone-before/after-rest ((voice 2)
				       (input-mode  () (ccl::mk-menu-subview :menu-list '(":beat" ":all" ":1st-beat")))
				       (gracenotes?  () :gracenotes?-exclude-mbox)
				       &optional
				       (rule-type  () :rule-type-mbox)
				       (weight 1)
				       (chord-voice 1))
	 "Tones (PC) in the given voice voice after a rest must be a member of the underlying chord PCs.

Args: 
voice: the voice to which this constraint is applied.
chord-voice (optional, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
	 ()
	 (r-pitch-pitch #'(lambda (pitches1 pitches2)	     
			    (let* ((voice-pitch1 (first pitches1))
				   (voice-pitch2 (first pitches2))
				   (rest1? (null voice-pitch1)) ; 1st note is rest
				   (rest2? (null voice-pitch2))) ; 2nd note is rest
			      (cond ((and rest1? rest2?) T) ; in case there are two rests in a row
				    (rest1? (PC-member voice-pitch2 (second pitches2))) ; after rest
				    (rest2? (PC-member voice-pitch1 (second pitches1))) 
				    (T T)))) ; before rest
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
					(if pitch ; no rest
					    (member (mod pitch 12)
						    (mapcar #'(lambda (chord-pitch) (mod chord-pitch 12)) 
							    (first chords)))
					  T))
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
			    (if (and (first pitches2) (second pitches2))  ;; no rests
				(let ((voice-pitch2 (second pitches2)))
				  (if (not (member (mod voice-pitch2 12) ; middle PC
						   ;; chord PCs
						   (mapcar #'(lambda (p) (mod p 12)) (first pitches2))))
				      (and (if (second pitches1) ; no rest
					       (<= (abs (- (second pitches1) voice-pitch2)) step-size)
					     T)
					   (if (second pitches3) ; no rest
					       (<= (abs (- voice-pitch2 (second pitches3))) step-size)
					     T))
				    T))
			      T))
			(list chord-voice voice)
			'(0)
			input-mode
			gracenotes?
			rule-type weight))



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
			    (if (first pitches) ; no rest
				(not (member (mod (first pitches) 12) 
					     (mapcar #'(lambda (p) (mod p 12)) 
						     (rest pitches))))
			      T))
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


;; TODO: 
;; - generalise for more voices 
(PWGLDef no-empty-sim-consonances-2parts 
	 ((voices 0)
	  (timepoints '(0))
	  (input-mode  10 (ccl::mk-menu-subview :menu-list '(":beat" ":all" ":1st-beat" ":1st-voice" ":at-timepoints")))
	  (gracenotes?  10 (ccl::mk-menu-subview :menu-list '(":exclude-gracenotes" ":include-gracenotes")))
	  &optional
	  (rule-type  10 (ccl::mk-menu-subview :menu-list '(":true/false" ":heur-switch")))
	  (weight 1))
	 "Disallows a perfect consonance between sim notes -- simple version for two parts.
All arguments are inherited from r-pitch-pitch."
	 () 
	 (r-pitch-pitch #'(lambda (pitches)
			    (if (first pitches) ; no rests 
				(let ((interval (mod (abs (- (first pitches) (second pitches))) 12)))
				  (not (member interval '(0 5 7))))
			      T))
			voices
			timepoints
			input-mode
			gracenotes?
			rule-type
			weight))

;;
;; Tintinnabuli rules 
;; 

#|
Ideas for using tintinnabuli in settings beyond the "Arvo Pärt sound". Doing the below in terms of software developmemt is easy -- think how to musically use this!?
 - M voice: allow for slightly larger skips (e.g., up to maj 3d?) 
 - T voice: control pitch with BPF?
 - ! Allow T voice and M voice to be pretty independent rhythmically, e.g.,
   - M-voice (very) slow like a cantus (i.e. stands out) -- by allowing for non-harmonic tones can form a  rich melody in terms of its [Tonvorrat]
   - T-voice much faster, like a figuration -- accompaniment (not Paert's ideal anymore, I guess, but in my own mind allow for that) 
   - There can be multiple somehow dependent or independent T-voices
   - M-voice and T-voice in different beat subdivisions or otherwise clearly separate
|#

(PWGLDef tintinnabuli-M-voice ((voice 0)
			       &optional
			       (max-interval 2)
			       (scale-voice 0))
	 "Rules for a tintinnabuli M-voice, inspired by Arvo Pärt (slightly generalised, because this rule is applicable over any harmony). The voice consists only of scale tones that move stepwise (max interval is whole tone).  

Args: 
voice: the number of the voice to constrain.
Optional:
max-interval (default 2): maximum interval in semitones.
scale-voice (default 0): the voice representing the underlying scale."
	 () 
	 (rules->cluster 
	  (min/max-interval voice :max-interval max-interval)
	  (only-scale-PCs voice :all :include-gracenotes
			  :true/false 1 scale-voice)))

(PWGLDef tintinnabuli-T-voice ((voice 0)
			       &key			       
			       (min-interval 3)
			       (max-interval 12)
			       (chord-voice 1))
	 "Rules for a tintinnabuli T-voice, inspired by Arvo Pärt (slightly generalised, because this rule is applicable over any harmony). The voice consists only of chord tones, and the minimum/maximum interval size can be controlled.  

Args: 
voice: the number of the voice to constrain.
Key args:
min-interval (default 3): minimum interval in semitones.
max-interval (default 12): maximum interval in semitones.
chord-voice (default 1): the voice representing the underlying chord."
	 () 
	 (rules->cluster 
	  (min/max-interval voice :min-interval min-interval :max-interval max-interval)
	  (only-chord-PCs voice :all :include-gracenotes
			  :true/false 1 chord-voice)))



