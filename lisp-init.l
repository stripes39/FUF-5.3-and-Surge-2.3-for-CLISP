;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:LISP -*-
;;; -----------------------------------------------------------------------
;;; File:         .lisprc
;;; Description:  Init file for Lucid CL. 
;;;               According to machine and lisp version, load appropriate
;;;               init file.
;;; Author:       Michael Elhadad
;;; Created:      25 Apr 90
;;; Modified:     
;;; Package:      LISP
;;; -----------------------------------------------------------------------

(in-package 'lisp)

;; We maintain a general init file for each version of Lucid: .lisprc2 
;; .lisprc3 and .lisprc4
;; In addition, there are specific init files for each type of machine: hp,
;; sun3 and sun4 (also specific to each version of lucid).  These files are
;; loaded AFTER the generic files have been loaded.


;; Need to define these variables because (machine-type) does not work.
(defvar *hp-machines* nil  "List of HP machines that can run LISP")
(setf *hp-machines* '(("fork")
		      ("open") 
		      ("pipe")
		      ("wait")
		      ("select")
		      ("moritz")
		      ("flims")
		      ("chamonix")))
#+hp(setf *machine-type* "hp")

(defvar *sun4-machines* nil  "List of sun4 machines that can run LISP")
(setf *sun4-machines* '(("hudson")
			("division")
			("mussel")
			("mistral")
			("jane")))
#+(and sun sparc)(setf *machine-type* "sun4")

(defvar *sun3-machines* nil "List of sun3 machines that can run LISP")
(setf *sun3-machines* '(("mermaid")
			("tuna")
			("beach")
			("siren")
			("malcolm")
			("lobster")
			("orca")
			("bivalve")))
#+(and sun mc68000)(setf *machine-type* "sun3")

;; General files first
;; For Version 4: NOTE THAT VERSION 3 FILES ARE STILL LOADED....
;; SO THIS MUsT COME AFTER VER 3 definitions.
#+lcl3.0(load "~/.lisprc3" :if-does-not-exist nil)
#+lcl4.0(load "~/.lisprc4" :if-does-not-exist nil)
#-lcl3.0 #-lcl4.0(load "~/.lisprc2" :if-does-not-exist nil)

;; Machine specific files next
#+lcl3.0(cond ((string= *machine-type* "sun3")
	       (load "~elhadad/.lisprc3-sun3" :if-does-not-exist nil))
	      ((string= *machine-type* "sun4")
	       (load "~elhadad/.lisprc3-sun4" :if-does-not-exist nil))
	      ((string= *machine-type* "hp")
	       (load "~elhadad/.lisprc3-hp" :if-does-not-exist nil))
	      (t (format t "The machine ~a is not defined in .lisprc")))

;; Version 4: NOTE THAT VERSION 3 FILES ARE STILL LOADED....
;; SO THIS MUsT COME AFTER VER 3 definitions.
#+lcl4.0(cond ((string= *machine-type* "sun3")
	       (load "~elhadad/.lisprc4-sun3" :if-does-not-exist nil))
	      ((string= *machine-type* "sun4")
	       (load "~elhadad/.lisprc4-sun4" :if-does-not-exist nil))
	      ((string= *machine-type* "hp")
	       (load "~elhadad/.lisprc4-hp" :if-does-not-exist nil))
	      (t (format t "The machine ~a is not defined in .lisprc")))

;; Lisp version 2 or older
#-lcl3.0 #-lcl4.0(cond ((string= *machine-type* "sun3")
	       (load "~elhadad/.lisprc2-sun3" :if-does-not-exist nil))
	      ((string= *machine-type* "sun4")
	       (load "~elhadad/.lisprc2-sun4" :if-does-not-exist nil))
	      ((string= *machine-type* "hp")
	       (load "~elhadad/.lisprc2-hp" :if-does-not-exist nil))
	      (t (format t "The machine ~a is not defined in .lisprc")))

			 
