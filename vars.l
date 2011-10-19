;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         vars.l
;;; Description:  Global variables used in FUF
;;; Author:       Michael Elhadad
;;; Created:      11 Feb 1992
;;; Modified:     20 Jul 1992: move defstructs here to ease compilation.
;;;               01 Jun 1993: make path-equal work for 2 symbols
;;;               09 Jun 1993: add ^~ notation in paths to escape from
;;;                            list with notation ^n~.
;;;               07 Jun 1994: add *conflate-leaves*
;;; Package:      FUG5
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

;; ============================================================
;; STRUCTURES
;; ============================================================
;;; Data structures for backtracking
;;- Frame: keeps a stack of frames representing the state
;;  of the computation at a backtraking point. That is all
;;  the variables on trace, undo and tests that are kept as one variable
;;  passed to all functions.
;;- Test: keeps a list of all the test statements met during unification
;;  of a branch to be tested at Determination-time. Path serves to keep
;;  the context of the statement within *input*.

(defstruct frame 
  (undo (list 'root))      ;; list of UNDOs
  (tests nil)              ;; all the TEST expressions met during unif.
  (trace-level 0)          ;; how many trace-flags are embedded
  (trace-flags nil)        ;; which trace-flags are active
  (name :anonymous))       ;; name of the frame

(defstruct test
  test                     ;; the sexpr to be evaluated
  path)                    ;; path to determine the pointer-references
                           ;; from the context of the test.

;; ------------------------------------------------------------
;; Define PATH structure, reader and writer plus handling functions.
;; ------------------------------------------------------------

(defstruct (path 
	    (:print-function print-path)
	    (:copier copy-path))
  l)

(defun print-path (path stream depth)
  (declare (ignore depth))
  (format stream "{~{~s~^ ~}}" (path-l path)))

(defun copy-path (path)
  (make-path :l (copy-tree (path-l path))))

(defun path-equal (p1 p2)
  (or (eq p1 p2)
      (and (path-p p1) (path-p p2) (equal (path-l p1) (path-l p2)))))

(defun equality (a b)
  (if (path-p a)
    (path-equal a b)
    (equalp a b)))

(defun curly-brace->path-reader (stream sub-char)
  "Reads {} into path structures."
  (declare (ignore sub-char))
  (make-path :l (normalize-path (read-delimited-list #\} stream t))))

(set-macro-character #\{ #'curly-brace->path-reader T)
;;; This prevents } from being part of form
(set-macro-character #\} (get-macro-character #\)))

(defun normalize-path (list &optional result &aux n)
  "Transforms JR's chars into their normal meaning:
   ^ and ^~ stand for themselves
   ^n stands for n ^ in sequence
   ^n~ stands for n ^ followed by ^~
   ~n stands for cdr n-1 times followed by car."
  (let* ((s (if (symbolp (car list)) (symbol-name (car list))))
	 (l (if s (length s))))
    (cond ((null list) (nreverse result))
	  ;; don't test for (eq '^) so that reader works even in packages
	  ;; that do not import fug5::^.
	  ((and (symbolp (car list))
		(string= s "^"))
	   (normalize-path (cdr list) (cons '^ result)))
	  ((and (symbolp (car list))
		(string= s "^~"))
	   (normalize-path (cdr list) (cons '^~ result)))
	  ((and (symbolp (car list))
		(char= (char s 0) #\^))
	   (setf n (parse-integer 
		    s :start 1 
		    :end (if (char= (char s (- l 1)) #\~) (- l 1) l)))
	   (normalize-path 
	    (cdr list) 
	    (nconc (if (char= (char s (- l 1)) #\~) (list '^~) nil)
		   (make-sequence 'list n :initial-element '^)
		   result)))
	  ((and (symbolp (car list))
		(char= (char (symbol-name (car list)) 0) #\~)
		(> (length (symbol-name (car list))) 1)
		(setf n (parse-integer (symbol-name (car list)) :start 1)))
	   (normalize-path
	    (cdr list)
	    (nconc (list 'car) 
		   (make-sequence 'list (- n 1) :initial-element 'cdr)
		   result)))
	  (t (normalize-path (cdr list) (cons (car list) result))))))



;; ============================================================
;; VARIABLES
;; ============================================================


(defvar *u-grammar* nil "The unification grammar")

(defvar *conflate-leaves* t 
  "When a path points to a leaf, copies the leaf or keep the pointer")

(defvar *disjunctive-attributes* '(alt opt ralt)
  "List of attributes defining a type of disjunction.")

;; Cset is added to this list when the cset parameter is known
(defvar *special-attributes* (append '(pattern fset control test) 
				     *disjunctive-attributes*)
  "List of attributes whose value is not a regular fd.
   Is updated when new unification method is defined.")

(defvar *lexical-categories*  
  '(verb noun adj prep conj relpro adv punctuation modal ordinal cardinal
	 phrase)
  "The Lexical Categories not to be unified")

(defvar *top* nil "Tracing in depth of top level")

(defvar *input* nil)

(defvar *cat-attribute* 'cat
  "The attribute marking the name of a category for constituents.")

(defvar *cset-attribute* 'cset
  "The attribute identifying the constituent set in an FD")

(defvar *added-cset* nil
  "Check if a cset has been added during unification.")

(defvar *fail* :fail "Value returned when failing")

(defvar *same* :same-path 
  "Value passed to backtracking points when failing at a non-leaf location")

(defvar dummy-frame (make-frame) "Used in undo-one")

;; How to recognize a path that requires intelligent backtracking:
;; it is declared in *bk-classes* along with its class.
(defvar *bk-classes* (make-hash-table))
(defvar *is-bk-failure* nil)
(defvar *failure-address* (make-path))
(defvar *class-of-failure* nil)
(defvar *changes-made* nil)

(defvar *ignore-control* nil "Whether controls are evaluated or not")
;; *ignore-control* should be t when normalizing an fd.

(defvar *any-at-unification* t "Whether tests for ANY are done at unifcation
  time or at determination time")

(defvar *use-given* t "Whether u(nil,given) fails or not.")
;; *use-given* should be nil when normalizing an fd or checking a grammar.

(defvar *use-any* t "Whether u(nil,any) fails")
;; *use-any* should be nil when normalizing an fd or checking a grammar.

(defvar *use-wait* t "Whether determine checks the waiting agenda or not.")

(defvar *agenda-policy* :force
  "Can be either :force or :keep.
   Determine what to do with frozen alt at the end of unification:
   - keep them unevaluated in result
   - force their evaluation.")

(defvar *fuf-print-warning* t
  "Whether to print warnings during runtime.")

;; -----------------------------------------------------------------------
(provide "vars")
;; -----------------------------------------------------------------------
