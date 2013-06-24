(in-package :cluster-rules)

(setf ccl::*pwgl-print-max-chars* 1000)


;; file-in-this-directory

(PWGLDef file-in-this-directory ((filename NIL))
	 "Returns a file in the same directory as the patch with the given filename (a string)."
	 ()
	 (make-pathname :directory (pathname-directory (ccl::pwgl-patch-pathname))
			:name filename))

;; read-lisp-file

(PWGLDef read-lisp-file ((path NIL))
	 "Expects a path name to a lisp file and returns the read (but not evaluated) content of the file. For example, if the file contains (1 2 3) then the list (1 2 3) is return (i.e., not a string, but a list, but 1 is not called as a function). Note that only the 1st Lisp form in path is returned."
	 ()
	 (if (probe-file path)
	     (with-open-file (stream path)
			     (read stream))
	   (warn "path ~A does not exist!" path)))


;; read-harmony-file

(defclass read-harmony-file-box (PWGL-box) ())

(defmethod patch-value ((self read-harmony-file-box) output)
  (let ((pos (position output (pwgl-outputs self)) ))
    (nth-output-patch-value self pos)))

(flet ((get-durs (plist dur-factor dur-key)
		 (let ((my-beat (getf plist :beat)))
		   (list (mapcar #'(lambda (dur) (* (/ dur my-beat) dur-factor)) (getf plist dur-key)))))
       (get-pitches (plist start-pitch pitch-key)
		    (list
		     (mapcar #'(lambda (pcs) 
				 (mapcar #'(lambda (pc) (+ pc start-pitch)) pcs)) 
			     (getf plist pitch-key)))))
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 0)))
    "scaledurs"
    (get-durs (read-lisp-file (nth-patch-value self 0))
	      (nth-patch-value self 1)
	      :scaledurs))
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 1)))
    "scalepitches"
    (get-pitches (read-lisp-file (nth-patch-value self 0))
		 (nth-patch-value self 2)
		 :scalepcss))   
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 2)))
    "chorddurs"
    (get-durs (read-lisp-file (nth-patch-value self 0))
	      (nth-patch-value self 1)
	      :chorddurs))
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 3)))
    "chordpitches"
    (get-pitches (read-lisp-file (nth-patch-value self 0))
		 (nth-patch-value self 2)
		 :chordpcss))) 


;; actual box def
(PWGLDef read-harmony-file ((path NIL) ; (nth-patch-value self 0)
			    (dur-factor 1) ; (nth-patch-value self 1)
			    (pc-transposition 60)) ; (nth-patch-value self 2)
  "Retrieves predefined harmony data from a lisp file for harmonic rules like only-scale-PCs and only-chord-PCs. This file must contain a list (as first lisp value) with the following format: a keyword-value list (plist) with the keys

  :beat (an int; defines subdivision of a beat, usually a quarter note. E.g., :beat 4 means that 2 indicates an eighth note)
  :scaledurs (list of ints; the durations of underlying scales)
  :scalepcss (list of list of ints; each sublist contains the set of scale pitch classes, root is first int)
  :chorddurs (list of ints, the durations of underlying chords)
  :chordpcss (list of list of ints; each sublist contains the set of chord pitch classes, root first int).

Intended use: connect the outputs to the inputs of clusterengine: scaledurs/scalepitches to the rhythm- and pitch domain of the 1st part and chorddurs/chordpitches to the 2nd part.

Args:
path: path to lisp file with format above.

dur-factor: fraction by which all chord/scale durations are multiplied.

pc-transposition: integer added to all chord/scale pitch classes. Useful for displying the result from clusterengine in a chord editor."
  (:class 'read-harmony-file-box :outputs '("scaledurs" "scalepitches" "chorddurs" "chordpitches")
   :groupings '(1 2))
  ())


#|
;;
;; Demo: How to define a box with multiple outputs
;; Def by Mikael Laurson, 18 May 2013 at pwgl-users@siba.fi
;;

(defclass my-multiout-box (PWGL-box) ())

(defmethod patch-value ((self my-multiout-box) output)
  (let ((pos (position output (pwgl-outputs self))))
    (nth-output-patch-value self pos)))

;; def for 1st output 
(defmethod nth-output-patch-value ((self my-multiout-box) (out (eql 0)))
  (+ (nth-patch-value self 0) (nth-patch-value self 1))); sum in1 in2

;; def for 2nd output
(defmethod nth-output-patch-value ((self my-multiout-box) (out (eql 1)))
  (+ (nth-patch-value self 0) (nth-patch-value self 1) (nth-patch-value self 2))) ; sum in1 in2 in3

;; ...
(defmethod nth-output-patch-value ((self my-multiout-box) (out (eql 2)))
  (+ (nth-patch-value self 0) (nth-patch-value self 2))) ; sum in1 in3

;; actual box def
(PWGLDef multiout-box ((a 0) (b 1) (c 2))
  "test multiout"
  (:class 'my-multiout-box :outputs '("1+2" "1+2+3" "1+3"))
  ())

|#


;;;
;;; List processing
;;;

(defun map-pairwise (fn xs)
  "Collects the result of applying the binary function gn on all pairwise combinations of elements in xs, i.e.,11 ((fn xs1 xs2) .. (fn xs1 xsN) (fn xs2 xs3) .. (fn xsN-1 xsN))."
  (if xs 
      (let ((x1 (first xs))
	    (xr (rest xs)))
	(append (mapcar #'(lambda (x2) (funcall fn x1 x2)) xr)
		(map-pairwise fn xr)))
    NIL))


