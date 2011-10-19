;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         FD2TEX.L
;;; Description:  Convert an FD to a LaTeX expression representing it in
;;;               array form. 
;;; Author:       Michael Elhadad
;;; Created:      19-Jan-97
;;; Modified:     
;;; Package:      FUG5
;;; Status:       Experimental
;;; -----------------------------------------------------------------------
;;;
;;; FUF - a functional unification-based text generation system. (Ver. 5.3)
;;;  
;;; Copyright (c) 19{87-97} by Michael Elhadad. all rights reserved.
;;;  
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
;;; -----------------------------------------------------------------------
;;; The type of output we want to get from an FD is:
;;; 
;;; \[ \left[ \begin{array}{ll}
;;;    semr & \left[ \begin{array}{ll}
;;;              assignments & [1] \left[ \begin{array}{ll}
;;;                            cat & set \\
;;;                            cardinality & 6 \\
;;;                            generic\_elt & \left[ \begin{array}{ll}
;;;                                          cat & assignment
;;;                                          \end{array} \right] \\
;;;                            \end{array} \right] \\
;;;              class & [2] \left[ \begin{array}{ll}
;;;                      cat & class \\
;;;                      name & ai
;;;                      \end{array} \right] \\
;;;              activity & [3] \left[ \begin{array}{ll}
;;;                         cat & assignment\_activity \\
;;;                         name & programming
;;;                         \end{array} \right] \\ 
;;;              relation1 & \left[ \begin{array}{ll}
;;;                          name & class\_assignt \\
;;;                          args & \left[ \begin{array}{ll}
;;;                                 class & [2] \\
;;;                                 assignt & [1]
;;;                                 \end{array} \right] \\
;;;                          \end{array} \right] \\
;;;              relation2 & \left[ \begin{array}{ll}
;;;                          name & assignt\_type \\
;;;                          args & \left[ \begin{array}{ll}
;;;                                 assignt & [1] \\
;;;                                 activity & [3]
;;;                                 \end{array} \right] \\
;;;                          \end{array} \right] \\
;;;              \end{array} \right] \\
;;; \end{array} \right] \]                         
;;;
;;; NOTES:
;;; 1. LaTeX has limits on the depth of embedding of matrices.
;;;    Therefore, if the FD is too deeply embedded, we need to split
;;;    it and put some reference as [n] plus a separate [n] = ... at
;;;    the toplevel.
;;; 2. First version without disjunctions. [TODO]
;;; 3. Limitation: if a path {a b c} appears in the FD, we assume 
;;;    there is a physical occurence ((a ((b ((c ...)))))).
;;;
;;; METHOD:
;;; 1. The regular step is:
;;;    FD = atom  LATEX = fd
;;;    FD = ((a1 v1) ...) LATEX = \left[ \begin{array}{ll} a1 & f(v1) \\ 
;;;                               ... \end{array} \right]
;;; 2. FD = ((a {path}) ...) LATEX = a & [n] 
;;;                          + before printing f(path), add [n].
;;; PROBLEMS:
;;; - Limited embedding in latex: 
;;; - How to deal with ((a {b})): need a physical rep for all paths
;;;   participating in a conflation.  But stop at the top conflation.
;;;   For example: ((a {b}) (b ((c 1)))) do not need labels 
;;;   for {a c}.
;;;   This could be done but it seems quite difficult.  Maybe could
;;;   just notice that it happens.

(in-package "FUG5")

(defvar *ref-counter* 0
  "The counter used for [n] references instead of paths.")

(defparameter *path-to-index* (make-hash-table :test #'equal)
  "A hashtable mapping path (just their list) to their [n] index in the FD")

(defparameter *latex-depth-limit* 6
  "Depth of embedding after which LaTeX explodes doing arrays.")
(defparameter *max-depth* (ceiling (/ *latex-depth-limit* 2)))

(defvar *quotient* nil
  "Quotient set of coreference links among paths of the FD")

(defun fd2tex (fd &optional (cat-attribute *cat-attribute*))
  "Convert an FD to a LaTeX expression encoding the FD in matrix form."
  (let* ((*input* (clean-fd (prep-input fd)))
	 (*ref-counter* 0)
	 (*cat-attribute* cat-attribute)
	 (*path-table* (make-hash-table :test #'equal :size 200))
	 (*path-table* (build-id-table *input* (make-path)))
	 (*quotient* (quotient-set *path-table*)))
    (declare (special *quotient* *cat-attribute*))
    (clrhash *path-to-index*)
    (setf fd (clean-fd (prep-input fd)))
    (tex-open-math)
    (l fd {})
    (tex-close-math)
    (values)))

(defun l (fd path)
  (let ((too-deep (reverse (l0 fd 1 path nil))))
    (fd2tex-extra too-deep)))

;; For each truncated subfd, write: [n] = ...
(defun fd2tex-extra (lpaths)
  (mapc #'(lambda (path)
	    (tex-close-math)
	    (format t "~%~%")
	    (tex-open-math)
	    (tex-path path)
	    (format t " = ")
	    (l (top-gdp *input* path) path)) 
	lpaths))

;; FD can be: atom or list of pairs or a path.
(defun l0 (fd level path too-deep)
  (cond ((leaf-p fd) (tex-fd fd) too-deep)
	((path-p fd) (tex-path fd) too-deep)
	(t (tex-open)
	   (indent level)
	   (l1 fd level path too-deep))))

;; FD is not an atom - iterate over list of pairs
(defun l1 (fd level path too-deep)
  (cond ((null fd) (tex-close) too-deep)
	(t (let ((deep1 (l2 (car fd) level path too-deep)))
	     (unless (null (cdr fd)) (tex-newline))
	     (indent level)
	     (l1 (cdr fd) level path deep1)))))

;; PAIR is (att FD)
(defun l2 (pair level path too-deep)
  (let ((newpath  (path-extend path (car pair)))
	(d1 (fd-depth (cadr pair))))
    (tex-fd (car pair))
    ;; (format t "========up ~s down ~s~%" level d1)
    (format t " & ")
    ;; Are we too-deep? put a tag
    ;; Try to cut deep fds in a balanced way ie. an fd of depth 8 is 
    ;; split as 2 fds of depth 4 not 6/2.
    ;; Only cut if too deep or if pair that we cut has a cat so that
    ;; we cut according to constituent structure
    (cond ((and (or (= level 6)
		    (and (> level *max-depth*)
			 (>= d1 *max-depth*)
			 ;; (>= level d1)
			 (pair-has-cat pair *cat-attribute*)))
		(not (path-p (cadr pair)))
		(not (leaf-p (cadr pair))))
	   ;; (format t "========CUT~%")
	   (tex-path newpath)
	   (cons newpath too-deep))
	  (t ;; need-tag before value?
	     ;; Does someone else point to me which is not simply
	     ;; an extension of a higher conflation?  And is my value just
	     ;; a path?
	     (if (and (path-is-conflation-site newpath *quotient*)
		      (not (path-p (cadr pair))))
		 (tex-path newpath))
	     (l0 (cadr pair) (+ 1 level) newpath too-deep)))))

;; FD is a path: decide how to encode it as [n] or \mbox{n} depending on level.
(defun tex-path (fd)
  (let ((tag (path-to-tag fd)))
    ;; (format t "\\fbox{~s}" tag)
    (format t "[~s]" tag)))

(defun tex-fd (fd)
  (cond ((stringp fd) (format t "\\mbox{``~a''}" fd))
	(t (format t "\\mbox{~(~s~)}" fd))))

(defun tex-newline ()
  (format t "\\\\[0.05cm]~%"))

(defun tex-open ()
  ;;  (format t "\\rule[.3cm]{0cm}{.2cm}\\left[ \\begin{array}{ll}~%")
  (format t "\\left[ \\begin{array}{ll}~%"))

(defun tex-close ()
  (format t "\\end{array} \\right]"))

(defun tex-open-math ()
  (format t "\\[~%"))

(defun tex-close-math ()
  (format t "\\]~%"))

(defun path-to-tag (path)
  (let ((rep (or (rep path *quotient*) path)))
    (multiple-value-bind (tag found) (gethash (path-l rep) *path-to-index*)
      (cond (found tag)
	    (t (setf (gethash (path-l rep) *path-to-index*) 
		     (incf *ref-counter*))
  	       *ref-counter*)))))

(defun indent (level)
  (format t "~&")
  (dotimes (i level) (format t "  ")))
  

;; How deep is an FD
(defun fd-depth (fd)
  (cond ((leaf-p fd) 0)
	((path-p fd) 0)
	(t (let ((d1 (fd-depth (cadar fd))))
	     (max (+ 1 d1) (fd-depth (cdr fd)))))))






