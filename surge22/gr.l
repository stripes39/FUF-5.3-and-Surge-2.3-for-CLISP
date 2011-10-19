;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         gr.l
;;; Description:  System file to load the grammar updated 
;;;               for testing measure, noun-compound, partitive & 
;;;               complex proper 
;;; Author:       Michael Elhadad
;;; Created:      17 Jul 1992
;;; Modified:     18 Jan 93: added adverbial, mood, special & nba (JR)
;;;                5 Jul 95: SURGE 2.2 VERSION
;;; -----------------------------------------------------------------------

(defun reload-gr ()
  "Reload all grammar"
    (load "surge22/tpat.l")
    (load "surge22/types.l")
    (load "surge22/transitivity.l")
    (load "surge22/voice.l")
    (load "surge22/adverbial.l")
    (load "surge22/clause.l")
    (load "surge22/mood.l")
    (load "surge22/verb-group.l")
    (load "surge22/np.l")
    (load "surge22/complex.l")
    (load "surge22/determiner.l")
    (load "surge22/gr-modular.l")
    (load "surge22/special.l")
    (load "surge22/nba.l"))

(defun load-gr ()
  "Load missing parts of the grammar"
  (let ((cur *default-pathname-defaults*)
	(*warn-if-no-in-package* nil))
    (cd "~elhadad/surge22/code")
    ;; Utilities for temporal patterns  (tpattern unifier)
    (require "tpat" "tpat.l")
    (require "types" "types.l")
    (require "clause" "clause.l")
    (require "verb-group" "verb-group.l")
    (require "np" "np.l")
    (require "determiner" "determiner.l")
    (require "complex" "complex.l")
    (require "gr-modular" "gr-modular.l")
    (require "nba" "nba.l")
    (require "special" "special.l")
    (cd cur)))
  
;;(load-gr)
(reload-gr)

;; ------------------------------------------------------------
(provide "$gr/gr")
;; ------------------------------------------------------------





