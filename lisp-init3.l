;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:LUCID-COMMON-LISP -*-
;;; -----------------------------------------------------------------------
;;; File:         .lisprc3
;;; Description:  Init file for Lucid CL. version 3.0
;;;               Redef. of load, require and cd to
;;;               expand environment variables.
;;; Author:       Michael Elhadad
;;; Created:      02-Feb-90
;;; Modified:     21-Nov-90 (Don't unintern cd anymore, just redefine it)
;;; Package:      LUCID-COMMON-LISP
;;; -----------------------------------------------------------------------

;;; This way all the functions are visible in all packages
(in-package 'lucid-common-lisp)

(setq *load-verbose* t)
;; Can be nil, :warn or :query
(setq *redefinition-action* nil)
(setq *load-source-pathname-types* '("l" "lisp" nil))
(setq *debug-print-length* nil
      *debug-print-level* nil
      *print-length* nil
      *print-level* nil)
(setq *load-binary-pathname-types* 
      (cond ((string= lisp::*machine-type* "hp")
	     '("b3" "b"))
	    ((string= lisp::*machine-type* "sun3")
	     '("lbin3" "lbin"))
	    ((string= lisp::*machine-type* "sun4")
	     '("sbin3" "sbin"))))

(defun lucid::prompt ()
  (format nil "~A> " (package-name *package*)))

(defun environment-p (string)
  "nil if string is not an environment variable.
       if it is, returns its value."
  (when (and (stringp string) (> (length string) 0) (eql (char string 0) #\$))
    (let ((value (environment-variable (subseq string 1))))
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
  (apply 'lucid-common-lisp::old-load (cons (expand-name name) args)))

(defun require (module &optional (file module))
  (funcall 'lucid-common-lisp::old-require module (expand-name file)))

(defun probe-file (filename)
  (funcall 'lucid-common-lisp::old-probe-file (expand-name filename)))

(unless (fboundp 'old-cd)
  (setf (symbol-function 'old-cd) (symbol-function 'cd)))

(defun system::cd (string)
  (lucid-common-lisp::old-cd (expand-name string)))

(export '(expand-name setenv))

;; -----------------------
;; Redefine shell environment variables for remote lisps
;; -----------------------

(defmacro setenv (var val)
  "Defines shell environment variable - var and val are strings"
  `(setf (environment-variable ,var) ,val))

;; Names of systems for use in Lisp and directory where they reside.
(setenv "abstrips" "/u/beach/elhadad/planner/djk")
(setenv "clisp" "/u/beach/radio/nikl")
(setenv "dgen" "/u/beach/radio/dgen")
(setenv "fug" "/u/beach/elhadad/Fug")
(setenv "fug2" "/u/beach/elhadad/Fug/new")
(setenv "fug3" "/u/beach/elhadad/Fug/typed")
(setenv "fug4" "/u/beach/elhadad/Fug/cut")
(setenv "fug5" "/u/beach/elhadad/Fug/freeze")
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
(setenv "classic" "/proj/beach/kr-system/classic")
(setenv "advisor" "/u/beach/elhadad/add/advisor")

(provide ".lisprc3")

