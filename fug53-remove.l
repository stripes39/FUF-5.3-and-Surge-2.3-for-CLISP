;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         fug53-remove.l
;;; Description:  System definition for FUG (Functional Unifier) with UNDO
;;;               and Charles Brendan's REMOVE special value
;;; Author:       Michael Elhadad
;;; Created:      05-Aug-88
;;; Modified:     13 Nov 89
;;;               31 Jan 90
;;;               30-Apr-90 - Defined *special-attributes*
;;;                         - Put all exports here
;;;               20 Feb 91 - added export for path functions
;;;               28 Jul 91 - update to FUG5 started
;;;               01 Jun 93 - added file findcset
;;;               06 Jun 93 - added path-value and set-path-value
;;;               09 Jun 93 - added ^~ notation.
;;;               04 Jan 94 - added fd-to-graph to package.
;;;               30 May 94 - added relocater to cset's definition.
;;;               08 May 95 - added make-load-form 
;;;                           (Murugan Kannan <kannan@steve.iit.edu>)
;;;               01 Dec 95 - load linearize2 (with HTML support)
;;;               04 Jan 96 - load graph3.l (with REMOVE support)
;;;               09 Jan 96 - load copypath (to complement REMOVE)
;;; Package:      FUG5
;;; -----------------------------------------------------------------------
;;;
;;; FUF - a functional unification-based text generation system. (Ver. 5.3)
;;;  
;;; Copyright (c) 19{87-95} by Michael Elhadad. all rights reserved.
;;;  
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
;;; -----------------------------------------------------------------------


(in-package "FUG5")
(format t "Loading system FUG5...~%")
(setf *print-array* t)  ;; for external

;; (setenv "fug5" "/u/beach/elhadad/Fug/freeze")

;; For BGU
(unless (common-lisp::environment-p "fug53")
  (setenv "fug53" "~elhadad/fuf/fuf53"))

;; On Poplog Common Lisp uncomment (Mark Evans):
;; (require "storeutils")
;; (setq *MAX-STORE-SIZE* 250000)

(require "$fug53/macros" "$fug53/macros.l")
(require "$fug53/vars" "$fug53/vars")
(require "$fug53/trace" "$fug53/trace")
(require "$fug53/generator" "$fug53/generator")
(require "$fug53/wait" "$fug53/wait")
(require "$fug53/define" "$fug53/define")
(require "$fug53/backtrack" "$fug53/backtrack")
(require "$fug53/external" "$fug53/external")
(require "$fug53/determine" "$fug53/determine")
(require "$fug53/type" "$fug53/type")
(require "$fug53/ignore" "$fug53/ignore")
(require "$fug53/alt" "$fug53/alt")
(require "$fug53/ralt" "$fug53/ralt")
(require "$fug53/fset" "$fug53/fset")
(require "$fug53/control" "$fug53/control")
(require "$fug53/graph3" "$fug53/graph3")
(require "$fug53/pattern" "$fug53/pattern")
(require "$fug53/path" "$fug53/path")
(require "$fug53/findcset" "$fug53/findcset")
(require "$fug53/top" "$fug53/top")
(require "$fug53/lexicon" "$fug53/lexicon")
(require "$fug53/linearize2" "$fug53/linearize2")
(require "$fug53/checker" "$fug53/checker")
(require "$fug53/fdlist" "$fug53/fdlist")
(require "$fug53/complexity" "$fug53/complexity")
(require "$fug53/continue" "$fug53/continue")
(require "$fug53/test" "$fug53/test")
;; (require "$fug53/fd-to-graph" "$fug53/fd-to-graph")
;; Fast (buggy) version of relocate/insert-fd
(require "$fug53/fd-to-graph2" "$fug53/fd-to-graph2")
(require "$fug53/copypath" "$fug53/copypath")

;; Set cset as a special feature
(define-procedural-type 'cset #'unify-cset 
  :syntax #'check-cset
  :relocater #'relocate-pattern)

;; On Allegro this allows you to compile source files containing {}
#+allegro
(defmethod make-load-form ((obj path) &optional env)
  (make-load-form-saving-slots obj))

#+lucid
(clos:defmethod lcl::make-load-form ((o lcl::structure-object))
  (lcl::make-load-form-saving-slots o))

(defun compile-fug5 ()
  "Compile all files required by system fug5"
  (let ((cur *default-pathname-defaults*))
    (proclaim '(optimize (compilation-speed 0) (speed 3) (safety 0) (debug 0)))
    (cd "$fug53/")
    (load "macros.l")
    (compile-file "vars.l")
    (compile-file "define.l")
    (compile-file "trace.l")
    (compile-file "generator.l")
    (compile-file "backtrack.l")
    (compile-file "external.l")
    (compile-file "fd-to-graph.l")
    (compile-file "determine.l")
    (compile-file "path.l")
    (compile-file "ignore.l")
    (compile-file "wait.l")
    (compile-file "alt.l")
    (compile-file "ralt.l")
    (compile-file "fset.l")
    (compile-file "control.l")
    (compile-file "type.l")
    (compile-file "pattern.l")
    (compile-file "findcset.l")
    (compile-file "graph3.l")
    (compile-file "top.l")
    (compile-file "lexicon.l")
    (compile-file "linearize2.l")
    (compile-file "checker.l")
    (compile-file "complexity.l")
    (compile-file "fdlist.l")
    (compile-file "continue.l")
    (compile-file "test.l")
    (compile-file "psgraph.l")
    (compile-file "copypath.l")
    (cd cur)))

(defun reload-backtrack ()
  "Reload files depending on the *fail* and backtrack macros only"
  (let ((cur *default-pathname-defaults*))
    (cd "$fug53/")
    (load "macros.l")
    (compile-file "backtrack.l")
    (load "backtrack")
    (compile-file "pattern.l")
    (load "pattern")
    (compile-file "alt.l")
    (load "alt")
    (compile-file "ralt.l")
    (load "ralt")
    (compile-file "graph3.l")
    (load "graph3")
    (compile-file "top.l")
    (load "top")
    (cd cur)))
  
(defun reload-fug5 ()
  "Reload all files of system in right order"
  (let ((cur *default-pathname-defaults*))
    (cd "$fug53/")
    (load "macros.l")
    (load "vars")
    (load "define")
    (load "generator")
    (load "trace")
    (load "backtrack")
    (load "external")
    (load "path")
    (load "fd-to-graph")
    (load "determine")
    (load "lexicon")
    (load "linearize2")
    (load "pattern")
    (load "ignore")
    (load "wait")
    (load "alt")
    (load "ralt")
    (load "fset")
    (load "control")
    (load "type")
    (load "findcset")
    (load "graph3")
    (load "top")
    (load "complexity")
    (load "fdlist")
    (load "continue")
    (load "checker")
    (load "copypath")
    (cd cur)))


(defun reload-bk ()
  "Reload all files depending on macro backtrack"
  (let ((cur *default-pathname-defaults*))
    (cd "$fug53/")
    (load "macros.l")
    (compile-file "alt.l")
    (compile-file "pattern.l")
    (compile-file "ralt.l")
    (compile-file "top.l")
    (load "alt")
    (load "pattern")
    (load "ralt")
    (load "top")
    (cd cur)))

(defun clean-fd (fd)
  (filter-flags (filter-nones fd)))

(defun fu (fd1 fd2)
  (clean-fd (u fd1 fd2)))

(defun funi (fd1 fd2)
  (clean-fd (uni-fd fd1 :grammar fd2)))

#+allegro
(import 'excl:exit)

(export '( ;; variables
	   *agenda-policy*
	   *all-trace-off*
	   *all-trace-on*
	   *any-at-unification*
	   *cat-attribute*
	   *check-ambiguities*
	   *cset*
	   *cset-attribute*
	   *default-external-function*
	   *dictionary*
	   *disjunctive-attributes*
	   *input*
	   *lexical-categories*
	   *special-attributes*
	   *top*
	   *trace-determine*
	   *trace-marker*
	   *typed-features*
	   *u-grammar*
	   *use-given*
	   *use-wait*

	   ;; functions
	   all-tracing-flags
	   avg-complexity
	   call-linearizer
	   categories-not-unified
	   check-cset
	   clean-fd
	   clear-bk-class
	   clear-grammar
	   clear-tests
	   complexity
	   control-demo
	   control-demo
	   def-alt
	   def-conj
	   def-grammar
	   def-test
	   define-bk-class
	   define-feature-type
	   define-procedural-type
	   disabled-tracing-flags
	   do-tenses
	   draw-grammar
	   draw-types
	   enabled-tracing-flags
	   external
	   fd-p
	   fd-sem
	   fd-syntax
	   fd-to-graph
	   FD-to-list 
	   FD-to-prolog 
	   filter-flags
	   filter-nils
	   filter-nones
	   fset
	   fuf-postscript
	   gdp
	   gdpp
	   get-test
	   grammar-p
	   hyper-trace-category
	   insert-fd
	   internal-trace-off
	   internal-trace-on
	   lexfetch
	   lexstore
	   list-cats
	   list-to-FD
	   normalize-fd
	   path-car
	   path-cdr
	   path-cons
	   path-butlast
	   path-last
	   path-append
	   path-push
	   path-pop
	   path-extend
	   path-null
	   path-equal
	   path-value
	   register-categories-not-unified
	   register-category-not-unified 
	   relativize-fd
	   relocate
	   reset-procedural-type
	   reset-procedural-types	   
	   reset-typed-features
	   set-path-value
	   store-plurals
	   store-verbs
	   subtype
	   subsume
	   sunder
	   top-fd-to-list
	   top-gdp
	   top-gdpp
	   trace-alts
	   trace-bp
	   trace-bk-class
	   trace-category
	   trace-cset
	   trace-disable
	   trace-disable-all
	   trace-disable-alt
	   trace-disable-match
	   trace-enable
	   trace-enable-all
	   trace-enable-alt
	   trace-enable-match
	   trace-level
	   trace-off
	   trace-on
	   trace-wait
	   types-postscript
	   u
	   u-exhaust
	   u-exhaust-top
	   u-rel
	   under
	   uni
	   uni-fd
	   uni-num
	   uni-string
	   unif
	   unify-cset
	   unregister-category-not-unified
	   test
	   test-with

	   ;; symbols explicitly used in the code
	   ;; (used with quote). Don't import them if already exist.
	   ==
	   ===
	   ;; * already exists in LISP
	   ;; trace already exists in USER
	   \@
	   ^
	   ^~
	   %TRACE-OFF%
	   %TRACE-ON%
	   %BREAK%
	   *done*
	   a-an 
	   adj
	   adv 
	   after
	   alt
	   an
	   animate
	   any
	   article 
	   before
	   capitalize
	   cardinal
	   case 
	   cat
	   conj 
	   consonant 
	   control
	   cset
	   demo-message
	   demonstrative 
	   det 
	   determiner 
	   digit
	   distance 
	   dots
	   dual
	   ending 
	   far 
	   feature
	   feminine 
	   first
	   gap
	   gender 
	   given
	   index
	   infinitive
	   interrogative
	   lex
	   masculine 
	   mergeable
	   modal 
	   mood
	   near
	   neuter
	   no
	   none
	   not-one
	   noun 
	   number 
	   objective 
	   opt
	   ordinal
	   past
	   past-participle
	   pattern
	   person 
	   personal 
	   phrase
	   plural 
	   possessive
	   pound
	   prep 
	   present
	   present-participle
	   present-third-person-singular 
	   pronoun 
	   pronoun-type 
	   punctuation
	   quantified
	   question 
	   ralt
	   reflexive 
	   relative
	   relpro 
           remove                           ;; CHARLES REMOVE
	   roman
	   root 
	   second
	   singular
	   subjective 
	   tense
	   test
	   third 
	   value
	   verb 
	   yes
	   ))

(provide "$fug53/fug5")

(defun warranty ()
  (format t 
    "
			    NO WARRANTY

  BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE PROGRAM ``AS IS'' WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
REPAIR OR CORRECTION.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING
OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED
TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY
YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGES.
    ")
  (values))

(defun license ()
  (format t 
    "
;;; FUF - a functional unification-based text generation system. (Ver. 5.3)
;;;  
;;; Copyright (c) 19{87-94} by Michael Elhadad. all rights reserved.
;;;  
;;; ================================================================
;;;         General License Agreement and Lack of Warranty 
;;; ================================================================
;;;
;;; This software is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY. The author does not accept responsibility to
;;; anyone for the consequences of using it or for whether it serves any
;;; particular purpose or works at all. No warranty is made about the
;;; software or its performance.  
;;; 
;;; Use and copying of this software and the preparation of derivative
;;; works based on this software are permitted, so long as the following
;;; conditions are met:
;;; 	o  The copyright notice and this entire notice are included intact
;;; 	   and prominently carried on all copies.
;;; 	o  No fees or compensation are charged for use, copies, or
;;; 	   access to this software. You may charge a nominal
;;; 	   distribution fee for the physical act of transferring a
;;; 	   copy, but you may not charge for the program itself. 
;;; 	o  If you modify this software, you must cause the modified
;;; 	   file(s) to carry prominent notices (a Change Log)
;;; 	   describing the changes, who made the changes, and the date
;;; 	   of those changes.
;;; 	o  Any work distributed or published that in whole or in part
;;; 	   contains or is a derivative of this software or any part 
;;; 	   thereof is subject to the terms of this agreement. The 
;;; 	   aggregation of another unrelated program with this software
;;; 	   or its derivative on a volume of storage or distribution
;;; 	   medium does not bring the other program under the scope
;;; 	   of these terms.
;;; 
;;; This software is made available AS IS, and is distributed without 
;;; warranty of any kind, either expressed or implied.
;;; 
;;; In no event will the author or his institutions be liable to you
;;; for damages, including lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of the use or
;;; inability to use (including but not limited to loss of data or data
;;; being rendered inaccurate or losses sustained by third parties or a
;;; failure of the program to operate as documented) the program, even if
;;; you have been advised of the possibility of such damanges, or for any
;;; claim by any other party. 
;;; 
;;; The current version of this software may be obtained by anonymous ftp
;;; from cs.columbia.edu (128.59.16.20) in the file pub/fuf/fuf5.3.tar.gz
;;; and from black.bgu.ac.il (132.72.40.7) in pub/fuf/fuf5.3.tar.gz.
;;;
;;; Please send bug reports, comments, questions and suggestions to
;;; elhadad@cs.bgu.ac..il. 
;;; I would also appreciate receiving any changes or improvements you may
;;; make.  
;;; 
    ")
  (values))


(format t 
    "
    FUF Version 5.3, Copyright (C) 1987-1994 Michael Elhadad.
    FUF comes with absolutely no warranty; for details type (fug5::warranty).
    This is free software, and you are welcome to redistribute it
    under certain conditions, type (fug5::license) for details.")



