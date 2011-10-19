;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:LUCID-COMMON-LISP -*-
;;; -----------------------------------------------------------------------
;;; File:         .lisprc4
;;; Description:  Init file for Lucid CL. version 4.0
;;;               NOTE THAT .lisprc3 is still loaded under 4.0
;;; Author:       Michael Elhadad
;;; Created:      02-Feb-90
;;; Modified:     21-Nov-90
;;; Package:      LUCID-COMMON-LISP
;;; -----------------------------------------------------------------------

;;; This way all the functions are visible in all packages
(in-package 'lucid-common-lisp)

(setq *load-verbose* t)
(setq *redefinition-action* nil)
(setq *load-source-pathname-types* '("l" "lisp" nil))
(setq *debug-print-length* nil
      *debug-print-level* nil
      *print-length* nil
      *print-level* nil)
(setq *load-binary-pathname-types* 
      (cond ((string= lisp::*machine-type* "hp")
	     '("b4" "b"))
	    ((string= lisp::*machine-type* "sun3")
	     '("lbin4" "lbin"))
	    ((string= lisp::*machine-type* "sun4")
	     '("sbin4" "sbin"))))

;; (load "~elhadad/Fug/xp/xp")
;; (format t "Use xp::pprint for a real pretty printer.~%")

(provide ".lisprc4")

