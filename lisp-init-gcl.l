;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:LISP -*-
;;; -----------------------------------------------------------------------
;;; File:         .lisprc-gcl
;;; Description:  Init file for GCL 2.1 (Gnu Common Lisp)
;;;               Redef. of load, require and cd to
;;;               expand environment variables.
;;; Author:       Michael Elhadad
;;; Created:      19-Aug-96
;;; Modified:     
;;; Package:      LISP
;;; -----------------------------------------------------------------------

;;; This way all the functions are visible in all packages
(in-package "LISP")

(setq *load-verbose* t)

(setq SYSTEM::*LOAD-TYPES* 
      (pushnew ".l" system::*load-types* :test #'string=))

(setq *debug-print-length* nil
      *debug-print-level* nil
      *print-length* nil
      *print-level* nil)

;; Since there is no way in GCL to change the value of a shell variable (no
;; setenv), I fake it here with an A-list.
(defvar *shell-environ* nil "Private cache of shell environment")

(defun setenv (var val)
  (pushnew (cons var val) *shell-environ* :test #'equal))

(defun environment-p (string)
  "nil if string is not an environment variable.
       if it is, returns its value in local cache if found, else in shell."
  (when (and (stringp string) (> (length string) 0) (eql (char string 0) #\$))
    (let ((value (or (cdr (assoc (subseq string 1) *shell-environ* 
				 :test #'string=))
		     (system::getenv (subseq string 1)))))
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
  (let ((name (expand-name name)))
    (apply 'lisp::old-load (cons  name args))))

(defun require (module &optional (file module))
  (funcall 'lisp::old-require module (expand-name file)))

(defun probe-file (filename)
  (funcall 'lisp::old-probe-file (expand-name filename)))

(unless (fboundp 'old-cd)
  (setf (symbol-function 'old-cd) (symbol-function 'system:chdir)))

(defun cd (string)
  (lisp::old-cd (expand-name string)))

;; Defpackage is not defined in gcl
(defmacro defpackage (&rest i) "nothing")

(in-package "USER")
(shadowing-import 'lisp::cd)

(in-package "LISP")
(export '(cd expand-name defpackage setenv))


(provide ".lisprc-gcl")

