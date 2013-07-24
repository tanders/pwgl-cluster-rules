;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem cluster-rules
  :description "Rules defined for the cluster-engine by Orjan Sandred." 
  :author "Torsten Anders"
  :version "0.001"
  :serial t ;; the dependencies are linear.
  :components ((:file "sources/package")
	       (:file "sources/utils")
	       (:file "sources/score")
	       ;; (:file "sources/generic-rules")
	       (:file "sources/rhythm-rules")
	       (:file "sources/melody-rules")
	       (:file "sources/harmony-rules")
	       (:file "sources/counterpoint-rules")
	       (:file "sources/export")
	       (:file "sources/menus"))
  :depends-on ("cluster-engine" ; "ta-utilities"
	       ))

