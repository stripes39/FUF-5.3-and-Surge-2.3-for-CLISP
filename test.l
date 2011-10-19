;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test.l
;;; Description:  Testing function
;;; Author:       Michael Elhadad
;;; Created:      19 Dec 1991
;;; Modified:     Aug 3 92: test-with use uni-string instead of uni-fd.
;;;               Aug 16 98: add limit as argument to test
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

;;; A package to test a sequence of cases and alert on mismatches
;;; Use def-test to define test cases.

(defvar *tests* (make-hash-table)
  "Hash table test-name to input / result")
(defvar *ordered-tests* nil 
  "The list of names of tests in the order where they have been defined")

(defmacro def-test (name result input)
  `(multiple-value-bind (test found) (gethash ',name *tests*)
     (declare (ignore test))
     (if found
       (format t "Redefining test ~s~%" ',name)
       (push ',name *ordered-tests*))
     (setf (gethash ',name *tests*) ',(cons result input))))
       

(defun get-test (test)
  (cdr (gethash test *tests*)))

(defun clear-tests ()
  "Reset test tables"
  (setf *ordered-tests* nil)
  (clrhash *tests*))

;; --------------------------------------------------
;; TEST
;; --------------------------------------------------
(defun uni-string-limit (fd limit)
  (uni-string fd :limit limit))

(defun test (&key (fun 'uni-string-limit) (test 'equalp) from to 
		  (timed nil) item (limit 1000))
  (let ((total 0)
	(correct 0)
	(problems nil))
    (format t "~%====================~%")
    (mapc #'(lambda (key)
	      (let* ((val (gethash key *tests*))
		     (input (cdr val))
		     (result (car val))
		     (ignore (format 
			      t "~&~s --> ~s~%" 
			      key (if (consp result) (car result) result)))
		     (new (if timed
			    (time (funcall fun input limit))
			    (funcall fun input limit))))
		(declare (ignore ignore))
		(incf total)
		(cond
		 ((and (consp result) (member new result :test test))
		  (format t "~s~%OK~%" new)
		  (incf correct))
		 ((and (not (consp result)) (funcall test new result))
		  (format t "OK~%")
		  (incf correct))
		 (t
		  (push key problems)
		  (cond ((consp result)
			 (format t "Expected one of:~%~{         ~s~%~}" 
				 result))
			(t (format t "Expected ~s~%" result)))
		  (format t "Instead  ~s~%" new))))
	      (format t "====================~%~%"))
	  (cond ((consp item) item)
		((null item) (extract-list from to *ordered-tests*))
		((symbolp item) (list item))))
    (format t "~%~%~s test~:P run - ~s correct.~%" total correct)
    (when problems
      (format t "The following tests are incorrect: ~s~%" 
	      (nreverse problems)))
    (values)))


(defun extract-list (from to list)
  ;; Deal with the reversing of the list (since ordered-tests is kepts in
  ;; reversed order)
  (let* ((list (reverse list))
	 (start (if from (position from list) 0))
	 (end   (if to (1+ (position to list :from-end t)))))
    (if (or (null end) (> end start))
      (subseq list start end)
      (subseq list end start))))


;; --------------------------------------------------
;; TEST-WITH
;; --------------------------------------------------
;; Test with the addition of a complement fd
;; For example: (test-with '((proc ((voice passive)))) :from t100)
(defun test-with (fd &key (from 1) (to 200) (timed nil)
		     (fun 'uni-string-limit) (item nil) (limit 1000))
  (let ((total 0))
    (format t "~%====================~%")
    (mapc 
     #'(lambda (test)
	 (let* 
	     ((val (gethash test *tests*))
	      (input (cdr val))
	      (result (car val))
	      (new (if timed
		     (time (funcall fun (u fd (prep-input input)) limit))
		     (funcall fun (u fd (prep-input input)) limit))))
	   (incf total)
	   (format t "~&~s --> ~s~%" test result)
	   (format t "Without fd: ~s~%With fd   : ~s~%" 
		   result new)
	   (format t "====================~%~%")))
     (cond ((consp item) item)
	   ((null item) (extract-list from to *ordered-tests*))
	   ((symbolp item) (list item))))
    (format t "~%~%~s test~:P run.~%" total)
    (values)))


;; --------------------------------------------------
;; DO-TENSES: iterate on all tenses, with negation, passive
;; --------------------------------------------------
(defun do-tenses (fd &key (from 1) (to 36)
		     (passive t)  (polarity t) (question t))
  "Take a clause fd and generate it at all tenses between from and to.
   If passive is non-nil, each tense is done active and passive.
   If polarity is non-nil, each tense is done both positive and negative."
  (when (symbolp fd)
    (multiple-value-bind (val found) (gethash fd *tests*)
      (if found
	(setf fd (cdr val))
	(return-from do-tenses (values)))))
  (do* ((i from (1+ i))
	(tense (or (find-symbol (format nil "TENSE-~s" i))
		   (find-symbol (format nil "tense-~s" i)))
	       (or (find-symbol (format nil "TENSE-~s" i))
		   (find-symbol (format nil "tense-~s" i)))))
       ((> i to) 
	(values))
       (format t "~%=======================~%")
       (format t "~s~%" tense)
       (let ((tfd (cons `(tense ,tense) fd)))
	 (uni tfd :limit 1000)
	 (when passive
	   (uni (u '((proc ((voice passive)))) tfd) :limit 1000))
	 (when polarity
	   (uni (cons '(polarity negative) tfd) :limit 1000))
	 (when question
	   (uni (cons '(mood yes-no) tfd) :limit 1000))
	 (when (and polarity passive)
	   (uni (u '((proc ((voice passive))) (polarity negative)) tfd)
		:limit 1000))
	 (when (and polarity question)
	   (uni (cons '(mood yes-no) 
		      (cons '(polarity negative) tfd)) :limit 1000))
	 (when (and polarity passive question)
	   (uni (u '((mood yes-no) (polarity negative) 
		     (proc ((voice passive)))) tfd)
		:limit 1000)))))


;; ============================================================
(provide "test")
;; ============================================================


