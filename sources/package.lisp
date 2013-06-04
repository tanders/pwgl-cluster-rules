;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(defpackage :cluster-rules
  (:nicknames :rule)
  (:use :common-lisp :cluster-engine :pw))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (import '(ccl::PWGLdef ccl::patch-value ccl::PWGL-box ccl::nth-patch-value ccl::pwgl-outputs ccl::add-PWGL-user-menu)
	  :cluster-rules))

