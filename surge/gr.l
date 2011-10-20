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
;;;               25 May 05: SURGE 2.3 VERSION
;;; -----------------------------------------------------------------------

(defun load-gr ()
  "Load all grammar files"
    (load "surge/gr-modular.l")
    (load "surge/tpat.l")
    (load "surge/types.l")
    (load "surge/transitivity.l")
    (load "surge/voice.l")
    (load "surge/adverbial.l")
    (load "surge/clause.l")
    (load "surge/mood.l")
    (load "surge/verb-group.l")
    (load "surge/np.l")
    (load "surge/complex.l")
    (load "surge/determiner.l")
    (load "surge/special.l")
    (load "surge/linearize")
    ;(load "surge/linearize.l" :external-format charset:iso-8859-1)
    (load "surge/lexicon.l")
    (load "surge/irregulars.l")
    (load "surge/nba.l"))
  
(defun compile-gr ()
    (compile-file "surge/linearize.l" :external-format charset:iso-8859-1))
  
(load-gr)

;; ------------------------------------------------------------
(provide "$gr/gr")
;; ------------------------------------------------------------
