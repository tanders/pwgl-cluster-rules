;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;;; *************************************************************
;;; Copyright (C) 2013 Torsten Anders (torsten.anders@beds.ac.uk) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; *************************************************************

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
		       (let ((dur-or-rest (* (if (zerop start) 1 (signum start)) ; sign of start
					     dur)))
			 (if rationalize? (rationalize dur-or-rest) dur-or-rest)))
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



(PWGLDef voice->velocities ((voice nil))
	 "Expects a voice and returns a list of its note velocities. If a score or part is given instead, then the first voice is selected. In case there is a rest, NIL is returned.
"
	 ()
	 (mappend #'(lambda (c) 
		      (cond ((ccl::rest-p c) '(NIL))
			    ((ccl::tied-p c) NIL)
			    (T (list (ccl::vel (first (ccl::collect-enp-objects c :note)))))))
		  (ccl::collect-enp-objects (first (ccl::collect-enp-objects voice :voice)) 
					    :chord)))


;;
;; Set clefs for a score
;;
;; Definitions inspired by Julien Vincenot
;;

(ccl::add-box-type :staff-types-scroll 
  `(ccl::mk-menu-subview :menu-list 
     ,(ccl::add-menu-list-keyword :staff-types '("treble-staff" "alto-staff" "tenor-staff" "bass-staff" "percussion-staff" "piano-staff"))
     :value 0))
(PWGLDef set-staff-clefs ((score '())
			  (clef 'treble-staff :staff-types-scroll) 
			  &rest (clefs 'treble-staff :staff-types-scroll))
	 "Set the clef of successive staves by extending and scrolling menus. Function destructively changes score.

Definition inspired Julien Vincenot"
	 (:groupings '(1 1))
	 (let ((all-clefs (cons clef clefs)))
	   (system::enp-script score        
			       (loop for clef in all-clefs
				     for index in (pw::arithm-ser 1 1 (length all-clefs))
				     collect 
				     `(* ?1 :partnum (list ,index)
					 (ccl::?if 
					  (setf (ccl::staff (ccl::read-key ?1 :part))
						(ccl::make-instance ',clef)))))
			       NIL)))


(PWGLDef set-staff-instruments ((score '()) 
				(instrument 'piano)
				&rest (instruments 'piano))
	 ;; (instruments () (ccl::mk-menu-subview :menu-list '(":beats" ":1st-beat")))
	 "Set the instruments of successive staves. Function destructively changes score."
	 (:groupings '(1 1))
	 (let ((all-instruments (cons instrument instruments)))
	   (system::enp-script score        
			       (loop for instr in all-instruments
				     for index in (pw::arithm-ser 1 1 (length all-instruments))
				     collect 
				     `(* ?1 :partnum (list ,index)
					 (ccl::?if 
					  (setf (ccl::instrument (ccl::read-key ?1 :part))
						(ccl::make-instance ',instr)))))
			       NIL)))


(PWGLDef set-staff-channels ((score '()) (channel 0) &rest (channels 0))
	 "Set the MIDI channels of successive staves. Function destructively changes score."
	 (:groupings '(1 1))
	 (let ((all-chans (cons channel channels)))
	   (system::enp-script score        
			       (loop for channel in all-chans
				     for index in (pw::arithm-ser 1 1 (length all-chans))
				     collect 
				     `(* ?1  :partnum (list ,index)
					 (?if (setf (ccl::chan ?1) ,channel))))
			       NIL)))

;; function copied from my ta-utilities library
(defun ensure-list (x)
  "Ensures that x is a list. If not, a list is wrapped around."
  (if (listp x)
    x
    (list x)))

;; function copied from my tot library
(defun circle-repeat (pattern n)
  "Circle through elements in pattern (a list) until n elements are collected.

  NOTE: only supports flat list so far.

* Arguments:
  - pattern: a list or single value treated as a one-value list
  - n: an integer

* Examples:

;;; (circle-repeat '(bb4 g4) 10)
;;; => (bb4 g4 bb4 g4 bb4 g4 bb4 g4 bb4 g4)

The function span can do something very similar. 

;;; (span (gen-repeat 10 'x) '(bb4 g4))


See also Opusmodus buildin gen-trim, which does the same, but is overall more flexible.
"  
  ;; (assert pattern
  ;;         (pattern) "circle-repeat: must be of at least length 1: ~A" pattern)
  (let* ((pattern-l (if pattern
			(ensure-list pattern)
			;; if pattern is nil then repeat nil
			(list nil)))
	 (l (length pattern-l)))
    (loop 
      for i from 0 to (- n 1)
      collect (nth (mod i l) 
                   pattern-l))))

; (circle-repeat '(bb4 g4 f4) 20)
; (circle-repeat nil 3)


(PWGLDef set-voice-velocities ((score '()) (velocities ()) &rest (velocity-lists ()))
	 "Set MIDI velocities of notes of successive voices. If a given velocities list is too long, the end is ignored, and if it is not long enough, then it is quasi looped. 
Function destructively changes score."
	 (:groupings '(1 1))
	 (when velocities
	   (let ((all-velocity-lists (cons velocities velocity-lists))
		 (voices (ccl:collect-enp-objects score :voice)))
	     (loop
		for voice in voices
		for velos in all-velocity-lists
		collect
		  (let ((notes (ccl:collect-enp-objects voice :note)))
		    (loop
		       for note in notes
		       for velo in (circle-repeat velos (length notes))
		       do (setf (ccl::vel note) velo)
		       collect note)))))
	 score)


#|
;;; TODO: 
(PWGLDef set-staff-velocity ((score '()) (velocities ()) &rest (velocity-lists ()))
	 "Set MIDI velocities of notes of successive staves. If the given list is too long, the end is chopped off, and if it is not long enough, then it is circled/looped."
	 (:groupings '(1 1))
	 (when velocities
	   (let ((all-velocity-lists (cons velocities velocity-lists)))
	     (system::enp-script score        
				 (loop for velos in all-velocity-lists
				    ;;; TODO: 
				    for index in (pw::arithm-ser 1 1 (length all-velocity-lists))
				    collect 
				      `(* ?1  :partnum (list ,index)
					  (?if (setf (ccl::chan ?1) ,channel))))
				 NIL))))
|#
