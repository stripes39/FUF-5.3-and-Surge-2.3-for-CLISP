;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:LISP -*-
;;; -----------------------------------------------------------------------
;;; File:         .lisprc2
;;; Description:  Init file for Lucid CL. Redef. of load, require and cd to
;;;               expand environment variables.
;;; Author:       Michael Elhadad
;;; Created:      15-Sep-88
;;; Modified:     01-Jan-89 (Define env. var for remote lisps)
;;;               02-Feb-90 (Make system:run-unix-program visible in lisp)
;;;                         (     system::quit ...)
;;;               25-Apr-90 (Made specific to lucid 2.0)
;;; Package:      LISP
;;; -----------------------------------------------------------------------

;;; This way all the functions are visible in all packages
(in-package 'lisp)

(setq *load-verbose* t)
(setq system:*redefinition-action* nil)
(setq system:*load-source-pathname-types* '(nil "l" "lisp"))
(setq system:*debug-print-length* nil
      system:*debug-print-level* nil)
(setq system:*load-binary-pathname-types* 
      (cond ((string= *machine-type* "hp")
	     '("b2" "b"))
	    ((string= *machine-type* "sun3")
	     '("lbin2" "lbin"))
	    ((string= *machine-type* "sun4")
	     '("sbin2" "sbin"))))

(defun environment-p (string)
  "nil if string is not an environment variable.
       if it is, returns its value."
  (when (and (stringp string) (> (length string) 0) (eql (char string 0) #\$))
    (let ((value (system:environment-variable (subseq string 1))))
      (if value value string))))

(defun home-dir-p (string)
  "nil if string is not a home dir segment in a pathname.
       if it is, returns the form (:HOME-DIR name)"
  (cond ((and (listp string) (eq :home-dir (car string))) string)
	((equal string "") nil)
	((and (stringp string) (eql (char string 0) #\~))
	 (list :home-dir (subseq string 1)))
	(t nil)))

(defun expand-dir (dir-in dir-out &aux value)
  "Expand all segments that need to be expanded. 
       Takes care of // and ~ business.
       dir-in is in the form of the output of pathname-dir.
       dir-out is the result (nil when first called)."
  (cond ((null dir-in) (nreverse dir-out))
	((equal (car dir-in) "")                    ; case of "//"
	 (expand-dir (cdr dir-in) nil))
	((setq value (home-dir-p (car dir-in)))     ; case of "~"
	 (expand-dir (cdr dir-in) (list value))) 
	((setq value (environment-p (car dir-in)))  ; case of "$"
	 (let ((expanded (pathname-directory 
			   (concatenate 'string value "/"))))
	   (cond ((eq :relative (car expanded))
		  (expand-dir (cdr dir-in) (append (reverse (cdr expanded))
						   dir-out)))
		 (t ;; expanded dir is absolute
		   (expand-dir (cdr dir-in) (reverse expanded))))))
	(t (expand-dir (cdr dir-in) (cons (car dir-in) dir-out)))))


(defun expand-name (filename)
  "Expand shell environment variables in file names"
  (setq filename (namestring filename))
  (cond	((eql (char filename (1- (length filename))) #\/) ; it's a dir
	 (namestring
	  (make-pathname :directory 
			 (expand-dir (pathname-directory filename) nil))))
	(t                                                ; it's a file
	 (let ((dir (expand-dir (pathname-directory 
				 (concatenate 'string filename "/")) nil)))
	   (namestring
	    (make-pathname :directory (butlast dir)
			   :name (car (last dir))))))))


(unless (fboundp 'old-load)
  (setf (symbol-function 'old-load) (symbol-function 'load)))
(unless (fboundp 'old-require)
  (setf (symbol-function 'old-require) (symbol-function 'require)))
(unless (fboundp 'old-probe-file)
  (setf (symbol-function 'old-probe-file) (symbol-function 'probe-file)))

(defun load (name &rest args)
  (apply 'lisp::old-load (cons (expand-name name) args)))

(defun require (module &optional (file module))
  (funcall 'lisp::old-require module (expand-name file)))

(defun probe-file (filename)
  (funcall 'lisp::old-probe-file (expand-name filename)))

(unless (fboundp 'old-cd)
  (setf (symbol-function 'old-cd) (symbol-function 'system::cd)))

(unintern 'system::cd (find-package "SYSTEM"))

(defun cd (string)
  (lisp::old-cd (expand-name string)))

(import 'system:run-unix-program)
(import 'system::quit)

(export '(expand-name cd run-unix-program quit))

;; -----------------------
;; Redefine shell environment variables for remote lisps
;; -----------------------

(defmacro setenv (var val)
  "Defines shell environment variable - var and val are strings"
  `(setf (system:environment-variable ,var) ,val))

;; Names of systems for use in Lisp and directory where they reside.
(setenv "abstrips" "/u/beach/elhadad/planner/djk")
(setenv "clisp" "/u/beach/radio/nikl")
(setenv "dgen" "/u/beach/radio/dgen")
(setenv "fug" "/u/beach/elhadad/Fug")
(setenv "fug2" "/u/beach/elhadad/Fug/new")
(setenv "fug3" "/u/beach/elhadad/Fug/typed")
(setenv "grapher" "/u/beach/elhadad/isi-grapher")
(setenv "kb" "/u/beach/radio/kb")
(setenv "lexint" "/u/beach/robin/radio")
(setenv "loom" "/u/beach/radio/loom")
(setenv "media" "/u/beach/radio/media")
(setenv "nikl" "/u/beach/radio/nikl")
(setenv "plan" "/u/beach/radio/plan")
(setenv "qpe" "/u/beach/radio/qpe")
(setenv "radio" "/u/beach/radio")
(setenv "util" "/u/beach/radio/util")
(setenv "advisor" "/u/beach/elhadad/advisor")

(provide ".lisprc")
