;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         LEXICON.L
;;; Description:  Lexicon for FUG5
;;; Author:       Jay Meyer 
;;; Created:      16-Mar-87
;;; Modified:     01-Jul-88 (Michael Elhadad)
;;;               30 Apr 90 - moved exports from here to fug5
;;; Package:      FUG5
;;; Status:       Experimental
;;; -----------------------------------------------------------------------
;;;
;;; FUF - a functional unification-based text generation system. (Ver. 5.3)
;;;  
;;; Copyright (c) 19{87-94} by Michael Elhadad. all rights reserved.
;;;  
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
;;; -----------------------------------------------------------------------

(in-package "FUG5")
(format t "Lexicon...~%")

;; -----------------------------------------------------------------------
;; Lexicon - JMyers comments as of 3/16/87
;; The lexicon contains only irregular plurals and irregular verb conjugations
;; [and pronouns - experimental]. It is accessed during the process of 
;; linearization.  Entries are represented as strings to control capitalization
;; and to allow the use of phrases.  There is no particular rationale for the
;; entries listed here and the lexicon should be extended as needed. 
;; -----------------------------------------------------------------------


;; ------------------------------------------------------------
;; GLOBAL VARIABLES
;; ------------------------------------------------------------

(defvar *dictionary* nil "The lexicon")
(defvar *irreg-plurals* nil 
  "List of singular-form paired with plural-form") 
(defvar *irreg-verbs* nil 
  "List of root verb paired with list of present-third-person-singular,
  past, present-participle and past-participle forms")
(defvar *pronouns-person* nil)
(defvar *pronouns-number* nil)
(defvar *pronouns-gender* nil)
(defvar *pronouns-case* nil)

(setq *irreg-plurals*
      ;; (key plural)
      ;; some pronouns are included to prevent pluralization
      '(("calf" "calves")
	("child" "children")
	("clothes" "clothes")
	("data" "data")
	("deer" "deer")
	("die" "dice")
	("elf" "elves") 
	("glasses" "glasses")
	("goods" "goods")
	("goose" "geese")
	("half" "halves") 
	("knife" "knives")
	("leaf" "leaves")
	("life" "lives")
	("loaf" "loaves")
	("man" "men")
	("many" "many") 
	("mouse" "mice")
	("oats" "oats")
	("ox" "oxen")
	("pants" "pants")
	("person" "people")
	("pliers" "pliers")
	("scissors" "scissors")
	("self" "selves")
	("sheaf" "sheaves")
	("sheep" "sheep")
	("some" "some")
	("thanks" "thanks")
	("that" "that")
	("them" "them")
	("there" "there")
	("these" "these")
	("they" "they")
	("thief" "thieves")
	("those" "those")
	("tongs" "tongs")
	("trousers" "trousers")
	("trout" "trout") 
	("which" "which")
	("wife" "wives")
	("woman" "women")))

;; (sort *irreg-plurals* #'string-lessp :key #'car) 

;; The irregular conjugation of "to be" is handled by a function 
;; in the linearizer (morph-be).

(setq *irreg-verbs*
;; (root present-third-person-singular past present-participle past-participle)
      '(("become" "becomes" "became" "becoming" "become")
	("buy" "buys" "bought" "buying" "bought")
	("come" "comes" "came" "coming" "come")
	("do" "does" "did" "doing" "done")
	("eat" "eats" "ate" "eating" "eaten")
	("fall" "falls" "fell" "falling" "fallen")
	("get" "gets" "got" "getting" "gotten")
	("give" "gives" "gave" "giving" "given")
	("go" "goes" "went" "going" "gone")
	("grow" "grows" "grew" "growing" "grown")
	("harbor" "harbors" "harbored" "harboring" "harbored")
	("have" "has" "had" "having" "had")
	("know" "knows" "knew" "knowing" "known")
	("leave" "leaves" "left" "leaving" "left")
	("make" "makes" "made" "making" "made")
	("see" "sees" "saw" "seeing" "seen")
	("take" "takes" "took" "taking" "taken")
	("throw" "throws" "threw" "throwing" "thrown")
	("undertake" "undertakes" "undertook" "undertaking" "undertaken")
	("write" "writes" "wrote" "writing" "written")))

(setq *pronouns-person*
      ;;(key(third) first second)
      '(("he" "I" "you")))

(setq *pronouns-gender*
      ;;(key(masculine) feminine neuter)
      '(("he" "she" "it")))

(setq *pronouns-number*
      ;;(key(sing) plural)
      '(("I" "we")
	("he" "they")
	("she" "they")
	("it" "they")))

(setq *pronouns-case*
;;(key(subjective) objective possessive reflexive)
      '(("I" "me" "mine" "myself")
	("you" "you" "yours" "yourself")
	("he" "him" "his" "himself")
	("she" "her" "hers" "herself")
	("it" "it" "its" "itself")
	("we" "us" "ours" "ourselves")
	("they" "them" "theirs" "themselves")))

;; Lexfetch is the function called during linearization 
;; to retrieve information from the lexicon 
(defun lexfetch (key property)
  "Retrieve information from the lexicon.
  Called during linearization.
  Properties can be: 
  verb : present-third-person-singular past present-participle past-participle
  noun : plural
  pronoun : subjective objective possessive reflexive."
  (cdr (safe-assoc property (gethash key *dictionary*))))


(defun lexstore (key property value)
  "Stores irregular lexical information into lexicon.
  For the possible properties cf lexfetch."
  (let ((oldvalue (lexfetch key property)))
       (if oldvalue 
	   (rplacd (safe-assoc property (gethash key *dictionary*)) value)
	   (setf (gethash key *dictionary*) 
		 (acons property value (gethash key *dictionary*))))))
 
(defun store-plurals (list)
  (mapc #'(lambda (x) (lexstore (car x) 'plural (cadr x)))
	list))

(defun store-verbs (list)
  (mapc #'(lambda (x) 
	    (let ((key (car x)))
	      (lexstore key 'present-third-person-singular (second x))
	      (lexstore key 'past (third x))
	      (lexstore key 'present-participle (fourth x))
	      (lexstore key 'past-participle (fifth x))))
	list))


(defun store-pronouns ()
  (mapc #'(lambda (x)
	    (let ((key (car x)))
	      (lexstore key 'first (second x))
	      (lexstore key 'second (third x))))
	*pronouns-person*)
  (store-plurals *pronouns-number*)
  (mapc #'(lambda (x)
	    (let ((key (car x)))
	      (lexstore key 'feminine (second x))
	      (lexstore key 'neuter (third x))))
	*pronouns-gender*)
  (mapc #'(lambda (x) 
	    (let ((key (car x)))
	      (lexstore key 'objective (second x))
	      (lexstore key 'possessive (third x))
	      (lexstore key 'reflexive (fourth x))))
	*pronouns-case*))


(defun initialize-lexicon (&optional size)
  (setq *dictionary* (make-hash-table :test #'equal
				      :size (cond (size) (t '200))))
  (store-plurals *irreg-plurals*)
  (store-verbs *irreg-verbs*)
  (store-pronouns))

(initialize-lexicon)


;; -----------------------------------------------------------------------
(provide "lexicon")
;; -----------------------------------------------------------------------
