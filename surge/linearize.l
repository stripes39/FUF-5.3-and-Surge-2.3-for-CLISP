;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         LINEARIZE.L
;;; Description:  Linearizer and Morphology for FUG5
;;; Author:       Michael Elhadad
;;; Created:      16-Oct-88
;;; Modified:     03-Nov-89 (ME: checked lex is a string - convert otw)
;;;               30 Apr 90: moved exports to fug5
;;;               10 May 90: path is a special type.
;;;               03 May 91: add support for ordinal and cardinal
;;;               05 May 91: allow for paths in patterns in linearize
;;;               21 May 91: added relative pronouns
;;;               16 Aug 91: fixed linearize to support paths in patterns.
;;;               18 Aug 91: use attr-p instead of symbolp
;;;               09 Oct 91: use digit inside cardinal to distinguish
;;;                          between 20 and twenty.
;;;               26 Dec 91: added fraction
;;;               27 Dec 91: accept specializations of plural.
;;;               28 Dec 91: accept specializations of pronoun.
;;;               21 Jul 92: add (digit roman) to cardinal and ordinal.
;;;               13 Sep 92: fixed punctuate to avoid Jr..
;;;               26 Oct 92: added capitalize processing.
;;;               22 Nov 92: fixed bug in punctuate with sequences of
;;;                          punctuation (ex: 114th St., Manhattan).
;;;               26 Oct 93: use cat-attribute instead of 'cat
;;;                          update callers of call-linearizer.
;;;               29 Dec 93: changed punctuate to avoid sentences finishing
;;;                          in ",".
;;;               01 Dec 95: Added HTML support (from a draft by Charles
;;;                          Brendan theorist@cs.utexas.edu)
;;;               25 Dec 95: Use subsume in morphology (recognize all
;;;                          specializations of normal cats)
;;;                          Do not add punctuation at the end of sentences
;;;                          if they already have one.
;;;               29 Aug 96: Added append-before and append-after support
;;;                          under punctuation to allow for things like
;;;                          $5 or 5F.  Check list-punctuation.
;;;               31 Mar 97: Added morphology for adjectives.
;;;                3 JUN 97: Added possessor-apostrophe
;;;                          Fixed: punctuate
;;;                                 morph-noun
;;;                                 plural-un/marking procedures
;;;               13 Sep 98: Filter "," from end of sentences.
;;;               25 May 05: SURGE 2.3 VERSION
;;;                          Added support for marking acronyms and contractions
;;;                          Added support for capitalization, including titles
;;;                          Added LaTeX functions similar to HTML
;;;                          Added checks for new morphological features
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

;(in-package :FUG5)
(format t "Linearizer with HTML/LaTeX support...~%")

;; ------------------------------------------------------------
;; Default on person is third, on number is singular, on tense is present
(defvar *indefinite-article* "a**"
  "An indefinite article when value of next word is still unknown
    (we don't know if it is a or an)")
(defvar *a-an-marker* "***"
  "Marker to add to a noun that must be preceded by a determiner an
   (like an acronym)")

;; CBC - Version 2.3: Add support for marking acronyms and contractions
(defvar *plural* "****"
  "Marker to add to a plural noun which ends with 's'")

(defvar *plural-acronym* "****acronym"
  "Marker to add to a plural acronym which ends with 's'")

(defvar *contraction-delete* nil)
(defvar *html-found* nil)

;;; Corrections for the POSSESSIVE APOSTROPHE
;; Possessive apostrophe is joined to a whole NP and not to a noun.
;; allow possessive apostrophe to appear after a whole noun phrase, not as a feature
;; of a noun.
;; Therefore, (feature possessive) is a feature of the NP and not of the head noun.
;; The mark is added as punctuation at the end of the NP.

(defun possessor-apostrophe (sign)
 (or (equal sign "*apostrophe*")
     ;; the need to mark with * here is that
     ;; bracket-open(close)-char functions recognizes
     ;; the ' as a bracket.
     (equal sign "*apostrophe-s*")
     (equal sign "*'s")
     (equal sign "*'")))

;; add it to the end of the word
(defun mark-noun-as-plural (word acronym)
  (if (equal acronym 'yes)
      (concatenate 'string word *plural-acronym*)
      (concatenate 'string word *plural*)))

(defun unmark-noun-as-plural (word)
  (let ((l (length word))
        (s (search *plural* word))
        (a (search *plural-acronym* word)))
    (if a
        (string-append (subseq word 0 a)
                       (subseq word (+ a (length *plural-acronym*)) l))
        (string-append (subseq word 0 s)
                       (subseq word (+ s (length *plural*)) l)))))
;    (subseq word  0 (- l (length *plural*)))))

(defun noun-marked-as-plural (word)
  (or (search *plural* word)
      (search *plural-acronym* word)))
;  (and (> (length word) (length *plural*))
;       (string= (subseq word (- (length word) (length *plural*)) (length word))  *plural*)))


;; ------------------------------------------------------------
;; TOP LEVEL : (call-linearizer fd)
;; Original by Jay Meyer (USC/ISI)
;; ------------------------------------------------------------
(defun call-linearizer (list &key (cat-attribute *cat-attribute*))
  (let ((*input* list)
        (*unknown-cat* nil))
    (declare (special *input* *unknown-cat*))
    ;; CBC - Version 2.3: Punctuation module needs to know if we are in html mode
    (setf *html-found* nil)
    (if (leaf-p list)
    (list "<fail>")
      (values
       ;; CBC - Version 2.3: Series of modifications for HTML, capitalization and punctuation
       (special-characters (capitalize (quote-punctuate (linear-punctuate (end-punctuate
         (remove ""
          (punctuate (linearize list (make-path) :cat-attribute cat-attribute)
                     ;; CBC - Version 2.3: Get question, formality, punctuation context
                     (and (subsume 'interrogative (second (safe-assoc 'mood list)))
                          (not (equal 'indirect (second (safe-assoc 'question-mode list)))))
                     (incr-gdp 'formality *input* (make-path))
                     (incr-gdp 'end-punctuation list (make-path))) :test #'equalp))))
          (incr-gdp 'capitalize-method list (make-path))))
       *unknown-cat*))))


;; CBC - Version 2.3:  Special Character processing for HTML
;; HTML browsers have special codes for things like e' in Italian
(defun special-characters (sentence)
  ;(format t "SpecChars:  ~S~%" sentence)
  (cond ((null *html-found*)  sentence)
        ((null sentence)  nil)
        ((or (search "á" (first sentence)) (search "Á" (first sentence))
             (search "é" (first sentence)) (search "É" (first sentence))
             (search "í" (first sentence)) (search "Í" (first sentence))
             (search "ó" (first sentence)) (search "Ó" (first sentence))
             (search "ú" (first sentence)) (search "Ú" (first sentence)))
            (cons (sc-aux (first sentence)) (special-characters (rest sentence))))
        (t  (cons (first sentence) (special-characters (rest sentence))))))

(defun sc-aux (str)
  (cond ((or (null str) (equal str ""))  "")
        ((equal (char str 0) #\á)  (string-append "&aacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\é)  (string-append "&eacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\í)  (string-append "&iacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\ó)  (string-append "&oacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\ú)  (string-append "&uacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\Á)  (string-append "&Aacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\É)  (string-append "&Eacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\Í)  (string-append "&Iacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\Ó)  (string-append "&Oacute;" (sc-aux (butfirst-char str))))
        ((equal (char str 0) #\Ú)  (string-append "&Uacute;" (sc-aux (butfirst-char str))))
        (t  (string-append (char str 0) (sc-aux (butfirst-char str))))))

;; ------------------------------------------------------------
;; CAPITALIZE and PUNCTUATE
;; Original written by Jay Meyer (USC/ISI)
;; ------------------------------------------------------------

;; CBC - Version 2.3: Delay capitalization till after initial punctuation
;;                    Also, new capitalization modes (see ir.l):
;;                    None, Second, All-Words, Every-Word, Title, Complete-Title,
;;                    First-One, First-Two, First-Three
(defun CAPITALIZE (sentence method &optional (start t))
  "Capitalizes the first word in a sentence."
  ;(format t "Capitalize:  ~S~%" sentence)
  (cond ((null sentence) sentence)
        ((equal (car sentence) "") (capitalize (cdr sentence) method start))
        ((eq method 'none)  sentence)
        ((equal #\< (char (car sentence) 0))
           (cons (car sentence) (capitalize (cdr sentence) method start)))
        ((and (not (equal (car sentence) " ")) (search " " (car sentence)))
           (capitalize (append (list (subseq (car sentence) 0 (search " " (car sentence))) " "
                                     (subseq (car sentence) (+ 1 (search " " (car sentence))) (length (car sentence))))
                               (rest sentence)) method start))
        ((equal (car sentence) "")  (capitalize (rest sentence) method start))
        ((or (equal #\\ (char (car sentence) 0))
             (equal #\" (char (car sentence) 0))
             (and (equal #\{ (char (car sentence) 0)) (equal #\\ (char (car sentence) 1)))
             (equal "-" (car sentence))
             (equal " " (car sentence))
             (equal "--" (car sentence))
             (equal "``" (car sentence))
             (and (equal 1 (length (car sentence))) (not (member2 (car sentence) '("I" "a" "A")))))
           (cons (car sentence) (capitalize (cdr sentence) method start)))
        ((and (= 1 (length (car sentence))) (equal "-" (third sentence)))
           (append (list (first sentence) " - ") (capitalize (cddr (cddr sentence)) method start)))
        ((and (equal (subseq (car sentence) 0 1) (string-downcase (subseq (car sentence) 0 1)))
              start (not (equal (car sentence) (string-downcase (car sentence)))))
           sentence)
        ((equal "'s" (car sentence))
           (cons (if (equal method 'all-words) "'S" "'s") (capitalize (cdr sentence) method start)))
        ((member2 (car sentence) '("T" "'t"))
           (cons (if (equal method 'all-words) "'T" "'t") (capitalize (cdr sentence) method start)))
        ((equal method 'second)
           (cons (car sentence) (capitalize (cdr sentence) 'first nil)))
        ((equal method 'all-words)
           (cons (string-upcase (car sentence)) (capitalize (cdr sentence) method nil)))
        ((equal method 'every-word)
           (cons (string-capitalize (car sentence) :end 1) (capitalize (cdr sentence) method nil)))
        ((equal method 'title)
           (cons (capitalize-title-words (car sentence) start) (capitalize (cdr sentence) method nil)))
        ((equal method 'complete-title)
           (cons (capitalize-title-words (car sentence) start 'all) (capitalize (cdr sentence) method nil)))
        ((equal method 'first-one)
           (cons (string-upcase (car sentence)) (cdr sentence)))
        ((equal method 'first-two)
           (cons (string-upcase (car sentence)) (cons (second sentence) (cons (string-upcase (third sentence)) (cdddr sentence)))))
        ((equal method 'first-three)
           (append (list (string-upcase (car sentence)) " ") (list (string-upcase (third sentence)) " ")
                   (list (string-upcase (fifth sentence))) (cdddr (cddr sentence))))
        (t  (cons (string-capitalize (car sentence) :end 1) (cdr sentence)))))

;; CBC - Version 2.3: Don't capitalize short words in titles
(defun capitalize-title-words (str start &optional all)
  (cond ((and (null start) (member2 str '("the" "a" "an" "and" "or" "but" "of" "in" "by" "at" "on" "with"))
              (not (equal all 'all)))       str)
        ((equal str (string-upcase str))  str)
        ((search "-" str)  str)
        (t  (string-capitalize str :end 1))))

(defun punctuation (letter)
  (member letter '(#\, #\. #\; #\: #\! #\?)))

;; CBC - Version 2.3: Extensive changes below for punctuation and morphology

(defun final-punctuation (letter)
  (member letter '(#\. #\! #\? #\:))) ;; removed #\, and #\;

(defun bracket-open-char (letter)
  (member letter '(#\( #\[ #\< #\" #\' #\` #\\ #\{)))

(defun bracket-close-char (letter)
  (member letter '(#\' #\` #\" #\> #\] #\) #\})))

(defun bracket-char (letter)
  (or (bracket-open-char letter)
      (bracket-close-char letter)))

(defun VOWEL (char)
  (member char '(#\A #\a #\E #\e #\I #\i #\O #\o #\U #\u)))

(defun butfirst-char (str)
  (if (< 0 (length str))
      (subseq str 1 (length str))))

(defun last-char (str)
  (if (< 0 (length str))
      (char str (- (length str) 1))))

(defun last2-str (str)
  (if (< 1 (length str))
      (subseq str (- (length str) 2) (length str))
      (last-char str)))

(defun last3-str (str)
  (if (< 2 (length str))
      (subseq str (- (length str) 3) (length str))
      (last2-str str)))

(defun last4-str (str)
  (if (< 3 (length str))
      (subseq str (- (length str) 4) (length str))
      (last3-str str)))

(defun butlast-char (str)
  (if (< 0 (length str))
      (subseq str 0 (- (length str) 1))
      ""))

(defun butlast2-char (str)
  (if (< 1 (length str))
      (subseq str 0 (- (length str) 2))
      (butlast-char str)))

(defun butlast3-char (str)
  (if (< 2 (length str))
      (subseq str 0 (- (length str) 3))
      (butlast2-char str)))

(defun punctuation-word (word)
  (and (stringp word)
       (= (length word) 1)
       (punctuation (char word 0))
       (not (equal (char word 0) #\,))))

;(defun punctuation-word (word)
;  (and (stringp word)
;       (= (length word) 1)
;       (punctuation (char word 0))))

(defun final-punctuation-word (word)
  (and (stringp word)
       (or (= (length word) 1)
           (equal word ".\"")
           (equal word ".''")
           (equal word "?\"")
           (equal word "--")
           (equal word "?''")
           (equal word "!\"")
           (equal word "!''"))
       (final-punctuation (char word 0))))

;; CBC - Version 2.3: Correctly interpret punctuation near quote marks
(defun quote-punctuate (sentence)
  ;(format t "QuotePunct:  ~S~%" sentence)
  (cond ((null sentence)  nil)
        ((and (equal (first sentence) " ") (< 0 (length (second sentence))) (equal #\, (char (second sentence) 0)))
            (append (list (second sentence)) (quote-punctuate (cddr sentence))))
        ((and (equal (first sentence) "''") (member2 (second sentence) '("," "." "!" "?")))
            ;; American quotes come after commas
            (append (list (second sentence) (first sentence)) (quote-punctuate (cddr sentence))))
        (t  (cons (first sentence) (quote-punctuate (rest sentence))))))

;; CBC - Version 2.3: Again punctuation near quote marks, including single quotes
(defun linear-punctuate (sentence)
  ;(format t "Linear-punctuate:  ~S, ~S~%" sentence (last sentence))
  (if (null sentence)  nil
      (let ((tag1  (first sentence))  (tag2  (second sentence))  (tag3  (third sentence)))
        (cond ((and (equal tag1 ".") (equal tag2 "''") (equal tag3 ","))
                  (cons ",''" (linear-punctuate (cdddr sentence))))
              ((and (equal tag1 ".''") (equal tag2 ","))
                  (cons ",''" (linear-punctuate (cddr sentence))))
              ((and (equal tag1 " ") (equal tag2 ":"))
                  (linear-punctuate (rest sentence)))
              ((and (equal tag1 "'") (equal tag2 ".''"))
                  (cons ".'''" (linear-punctuate (cddr sentence))))
              ((and (not (member2 tag1 '(" " "``"))) (equal tag2 "`"))
                  (cons tag1 (cons " `" (linear-punctuate (cddr sentence)))))
              ((and (equal tag1 "''") (equal tag2 " ") (equal tag3 ",''"))
                  (cons tag3 (linear-punctuate (cdddr sentence))))
              ((equal tag1 "*'")  (cons "'" (linear-punctuate (rest sentence))))
              ((and (equal tag1 ",") (< 1 (length tag2)) (equal "</" (subseq tag2 0 2)))
                  ;; punctuation before end html/xml tags should go after
                  (cons tag2 (cons tag1 (linear-punctuate (cddr sentence)))))
              (t  (cons tag1 (linear-punctuate (rest sentence))))))))

;; CBC - Version 2.3: Correctly fix punctuation at the end of the sentence
;;                    Many of these derive from examples in the Penn TreeBank
(defun end-punctuate (sentence)
  ;(format t "End-punctuate:  ~S, ~S~%" sentence (last sentence))
  (cond ((null sentence)  nil)
        ((listp (first sentence))  (list (first (end-punctuate sentence))))
        ((equal (last sentence) '(""))  (end-punctuate (butlast sentence)))
        ((equal (last sentence) '("::"))  (end-punctuate (append (butlast sentence) '(":"))))
        ((equal (last sentence 2) '(":" ":"))  (end-punctuate (butlast sentence)))
        ((equal (last sentence 3) '(":" " " ":"))  (end-punctuate (butlast sentence 2)))
        ((and (equal (last sentence) '("?")) (equal (first (last sentence 2)) ".''"))
            (end-punctuate (append (butlast sentence 2) '("?''"))))
        ((equal (last sentence) '(","))  (end-punctuate (append (butlast sentence) '("."))))
        ((equal (last sentence 2) '(" " ".''"))  (end-punctuate (append (butlast sentence 2) '(".''"))))
        ((equal (last sentence 3) '(" " "''" "."))  (end-punctuate (append (butlast sentence 3) '(".''"))))
        ((equal (last sentence 4) '(" " "''" " " "."))  (end-punctuate (append (butlast sentence 4) '(".''"))))
        ((equal (last sentence 2) '("''" ".''"))  (end-punctuate (append (butlast sentence 2) '(".''"))))
        ((equal (last sentence 5) '("''" " " "''" " " "."))  (end-punctuate (append (butlast sentence 5) '(".''"))))
        ((equal (last sentence 2) '("''..." "."))  (end-punctuate (append (butlast sentence 2) '("....''"))))
        ((equal (last sentence 2) '("''" "..."))  (end-punctuate (append (butlast sentence 2) '("...''"))))
        ((equal (last sentence 2) '("'" "."))  (end-punctuate (append (butlast sentence 2) '(".'"))))
        ((and (member2 (first (last sentence)) '("\"" "''"))  ;"." "?" "!"
              (< 1 (length (first (last sentence 2))))
              (equal #\< (char (first (last sentence 2)) 0))
              (equal #\/ (char (first (last sentence 2)) 1)))
            (end-punctuate (append (butlast sentence 2) `("." ,(first (last sentence 2))))))
        ((and (equal (last sentence) '(" ")) (equal (first (last sentence 2)) "''"))
            (end-punctuate (append (butlast sentence 2) '("''"))))
        ((and (equal (last sentence) '("?")) (equal (first (last sentence 2)) ")"))
            (end-punctuate (append (butlast sentence 2) '("?" ")"))))
        ((and (equal (last sentence) '(".")) (< 2 (length (first (last sentence 2))))
              (equal (char (first (last sentence 2)) (- (length (first (last sentence 2))) 1)) #\.)
              (not (equal (last2-str (first (last sentence 2))) "..")))
            (butlast sentence))
        ((equal (last sentence 2) '(" " "."))  (end-punctuate (append (butlast sentence 2) '("."))))
        ((equal (last sentence 2) '("." "."))  (end-punctuate (append (butlast sentence 2) '("."))))
        ((equal (last sentence 2) '("" "."))  (end-punctuate (append (butlast sentence 2) '("."))))
        ((equal (last sentence 2) '("'" "!"))  (end-punctuate (append (butlast sentence 2) '("!'"))))
        (t  sentence)))


;; CBC - Version 2.3: Extensive changes to sentence-internal punctuation code

;; Rules:
;; 0/ deal with "an empty" "an RST relation"
;; 1/ don't put spaces before closing brackets.
;; 2/ don't put spaces after opening brackets.
;; 3/ don't put spaces before punctuation.
;; 4/ filter out ",p" into "p" for every punctuation p
;; 5/ filter out ".p." into ".p" except ".." into "."
;; 6/ for all other "pq", "q"
;; 7/ check aphostophe sign
;; 8/ capitalize after an interjection ending in "!"
(defun PUNCTUATE (sentence question-flag formality end-punctuation)
  ;(format t "Punctuate:  ~S~%" sentence)
  (setf sentence (remove "" sentence :test #'equalp))

  ;; Remove punctuation from beginning of sentence
  (if (and (equal "\"" (first sentence)) (equal "," (second sentence)))
      (setf sentence (cons (first sentence) (cddr sentence))))

  ;; Remove punctuation at beginning of sentence
  (while (and sentence (punctuation (char (car sentence) 0)))
    (let ((x (car sentence)))
      (while (and (not (equal x "")) (punctuation (char x 0)))
         (setq x (subseq x 1)))
      (if (equal x "")
          (setf sentence (cdr sentence))
          (setf (car sentence) x))))

  ;; Add punctuation at end of sentence if necessary.
  ;; Hard to do that in grammar because does not know if constituent is
  ;; embedded or not - so leave it to here.
  ;; Only do that if there is not already a punctuation element at the end
  ;; of the sentence
  (unless (final-punctuation-word (car (last sentence)))
      (setf sentence (append sentence (list
         (cond ((equal 'none end-punctuation) "")
               ((member2 (car (last sentence)) '("--")) "")
               ((and (stringp end-punctuation) (not (equal 0 (length end-punctuation)))) end-punctuation)
               (question-flag "?")
               (t  "."))))))

  ;; Will be true when we detect a pair (x=noun-marked-plural y=possessor-mark)
  ;; This way, next time we come, we'll have (x=possessor-mark y=any)
  ;; and we remember what was preceeding the possessor-mark.
  (let ((*possessor-plural* nil)
    (*possessor-plural-mark* "*'")
    (*possessor-singular-mark* "*'s")
    (*spaceless-comma-mark* "*,")
    (*capitalize-after-interjection* nil))
    (declare (special *possessor-plural* *possessor-plural-mark* *spaceless-comma-mark*
                      *possessor-singular-mark* *capitalize-after-interjection*))
    (if (null sentence)
    (list "<fail>")
      (append
       (mapcan
    #'(lambda (x y z)
        ;(format t "WORDS:  ~S,  ~S,  ~S~%" x y z)
        (when (noun-marked-as-an x)
          (setq x (unmark-noun-as-an x)))
        (when *capitalize-after-interjection*
          ;(format t "Capitalize after Interjection:  ~S,  ~S,  ~S~%" x y z)
          (setq x (string-capitalize x :end 1))
          (setq *capitalize-after-interjection* nil))
        (when (possessor-apostrophe x)
          (cond ((equal x "*apostrophe-s*") (setq x *possessor-singular-mark*))
                (*possessor-plural* (setq x *possessor-plural-mark*))
                (t  (setq x *possessor-singular-mark*))))
        (if (noun-marked-as-plural x)
            (progn (setf *possessor-plural* (possessor-apostrophe y))
                   (setq x (unmark-noun-as-plural x)))
            (if (or (equal (last-char x) #\s) (equal (last-char x) #\z))
                (setf *possessor-plural* (possessor-apostrophe y))))
        (cond
         ;; Capitalize after double colon rule
         ((equal x "::")
          ;(format t "PUNCTUATE:  Fired Colon Capitalization Rule: ~s ~s ~s~%" x y z)
          (setq *capitalize-after-interjection* nil) (list ":" " "))

         (*contraction-delete*  (setf *contraction-delete* nil) nil)
         ;; Rule 0
         ((equal x *indefinite-article*)
          ;(format t "PUNCTUATE:  Fired Rule 0: ~s ~s~%" x y)
          (cond ((and (< 3 (length y))
                      (or (string= (string-downcase y) "one-" :end1 4)
                          (string= (string-downcase y) "eu" :end1 2)
                          (string= (string-downcase y) "us" :end1 2)
                          (string= (string-downcase y) "U." :end1 2)
                          (string= (string-downcase y) "uni" :end1 3)))
                     (list "a" " "))
                ((or (vowel (elt y 0))
                     (member2 y '("hour" "hourlong" "hourly"))
                     (noun-marked-as-an y))
                    (if (equal y "other") (list "an" "")  (list "an" " ")))
                ((and (< 0 (length y)) (equal (subseq y 0 1) "8"))  (list "an" " "))
                ((and (< 1 (length y)) (not (= 3 (length y))) (member2 (subseq y 0 2) '("11" "18")))  (list "an" " "))
                ((and (< 1 (length y)) (search (subseq y 0 1) "$#") (equal (subseq y 1 2) "8"))  (list "an" " "))
                ((and (< 2 (length y)) (search (subseq y 0 1) "$#") (member2 (subseq y 1 3) '("11" "18")))  (list "an" " "))
                (t  (list "a" " "))))

         ;; Follows a contraction rule
         ;; HTML/XML rules
         ;((and (< 0 (length x)) (equal (char x 0) #\<) (equal (char x (- (length x) 1)) #\>))
         ;   (format t "XML1:  ~S,  ~S~%" x y)
         ;   (if (equal (char x 1) #\/) (list x "") (list "" x)))
         ;((and (< 0 (length y)) (equal (char y 0) #\<) (equal (char y (- (length y) 1)) #\>))
         ;   (format t "XML2:  ~S,  ~S~%" x y)
         ;   (if (equal (char y 1) #\/) (list x "") (list x "" y)))
         ((and (< 0 (length x)) (equal (char x 0) #\<) (equal (char x (- (length x) 1)) #\>))
            ;(format t "HTML1:  ~S,  ~S~%" x y)
            (setf *html-found* t)
            (if (and (equal (char x 1) #\/) (< 1 (length y)) (not (equal (char y 1) #\/)))
                (list x " ") (list x)))
         ((and (< 0 (length y)) (equal (char y 0) #\<) (equal (char y (- (length y) 1)) #\>))
            ;(format t "HTML2:  ~S,  ~S~%" x y)
            (setf *html-found* t)
            (if (equal (char y 1) #\/) (list x "") (list x " ")))
         ;((and (< 0 (length y)) (equal (char y 0) #\<) (equal (char y (- (length y) 1)) #\>))
         ;   (format t "HTML/XML3:  ~S,  ~S~%" x y)
         ;   (if (equal (char y 1) #\/) (list y "") (list "" y)))

         ;; Rule for no spaces after commas (e.g., latex bibliography lists)
         ((equal x *spaceless-comma-mark*)
            (list ","))
         ((equal y *spaceless-comma-mark*)
            (list x))

         ;; Rule 0b
         ((equal x (string-capitalize *indefinite-article* :end 1))
          ;(format t "PUNCTUATE:  Fired Rule 0b: ~s ~s~%" x y)
          (if (or (vowel (elt y 0))
              (noun-marked-as-an y))
              (if (equal y "other")
                  (list "An" "")
                  (list "An" " "))
              (list "A" " ")))

         ;; Rule single-quote
         ((and (< 1 (length y)) (equal (elt y 0) #\') (not (equal (elt y 1) #\')) (not (equal x " ")))
            ;(format t "PUNCTUATE:  Fired Rule single-quote: ~s ~s~%" x y)
            (list x " "))
         ;; Rule quotation 1
         ((and (equal x ",") (or (equal y "\"") (equal y "#")))
          ;(format t "PUNCTUATE:  Fired Rule quotation 1: ~s ~s~%" x y)
          (list "," " "))
         ;; Rule quotation 2
         ((and (or (equal y "``") (equal x "''")) (not (equal x *possessor-singular-mark*)))
          ;(format t "PUNCTUATE:  Fired Rule quotation 2: ~s ~s ~s~%" x y z)
          (if (equal y ",") (list x) (list x " ")))
         ;; Rules 1 and 2
         ((and (not (equal x "")) (not (equal y "")) (not (equal x *possessor-singular-mark*))
               (not (equal (char x 0) #\')) ;; ex.: '82 Ford --> '82Ford
               (or (bracket-open-char (char x 0))
                   (bracket-close-char (char y 0))))
          ;(format t "PUNCTUATE:  Fired Rules 1 and 2: ~s ~s~%" x y)
          (list x))

         ;; Rule 8
         ((and (not (equal x "")) (not (equal y ""))
               (final-punctuation (char x 0))
               (equal (length x) 1)
               (not (punctuation (char y 0)))
               (not (search " " y)))
            (setf *capitalize-after-interjection* t)
            ;(format t "PUNCTUATE:  Fired Rule 8: ~s ~s ~s~%" x y z)
            (if (member2 z '("a.m." "p.m." "AM" "PM"))
                (list x)
                (list x " ")))
         ;; Rule 4
         ((and (not (equal x "")) (not (equal y ""))
               (equal (last-char x) #\,)
               (punctuation (char y 0)))
                    ;(list (butlast-char x)))
          ;(format t "PUNCTUATE:  Fired Rule 4: ~s ~s ~s~%" x y z)
          (list ""))
         ;; Rule 5
         ((and (not (equal x "")) (not (equal y ""))
               (equal (last-char x) #\.)
               (punctuation (char y 0))
               (not (equal (char y 0) #\.)))
          ;(format t "PUNCTUATE:  Fired Rule 5: ~s ~s ~s~%" x y z)
          (list x))
         ;; Rule 6
         ((and (not (equal x "")) (not (equal y ""))
               (punctuation (last-char x))
               (punctuation (char y 0)))
          ;(format t "PUNCTUATE:  Fired Rule 6: ~s ~s ~s~%" x y z)
          (list (butlast-char x)))
         ;; Rule 7
         ((possessor-apostrophe y)
          ;(format t "PUNCTUATE:  Fired Rule 7-a: ~s ~s~%" x y)
          (list x))
         ((possessor-apostrophe x)
          ;(format t "PUNCTUATE:  Fired Rule 7-b: ~s ~s~%" x y)
          (if (and (not (equal x "")) (not (equal y ""))
                   (punctuation (char y 0)))
              (list (subseq x 1 (length x)))
              (list (subseq x 1 (length x))  " ")))
         ;; Rule 3
         ((and (not (equal x "")) (not (equal y ""))
               (punctuation (char y 0)))
          ;(format t "PUNCTUATE:  Fired Rule 3: ~s ~s~%" x y)
          (list x))
         ;; Rule for Special Symbols
         ((and (not (equal x "")) (search y "%^*$#@"))
          ;(format t "PUNCTUATE:  Fired Special Symbol Rule: ~s ~s~%" x y)
          (list x))
         ;; Contractions in informal sentences.
         ((or (and (equal formality 'informal)
                   (or (member2 (string-downcase x) '("it" "he" "she" "what" "there" "where"))
                       (not (member2 (string-downcase z) '("," "a**" "the"))))
                   (not (and (member2 (string-downcase y) '("will" "had" "has" "have" "would" "should" "could"))
                             (member2 (string-downcase z) '("not" "," "." "?" "!" ")"))))
                   (or (not (member2 (string-downcase x) '("he" "that" "there")))
                       (not (and (member2 (string-downcase z) '("not" ",")) (not (member2 (string-downcase x) '("he" "that")))))
                       (not (and (member2 (string-downcase y) '("will")) (not (member2 (string-downcase x) '("that")))))))
              (and (equal formality 'informal)  ;; Specific known exceptions go below
                   (not (and (member2 (string-downcase x) '("that")) (member2 (string-downcase y) '("will")) (member2 (string-downcase z) '("not"))))
                   (or (member2 (string-downcase x) '("that")) (member2 (string-downcase y) '("is")) (member2 (string-downcase z) '("the"))
                       (member2 (string-downcase x) '("was")) (member2 (string-downcase y) '("not")))))
             ;(format t "PUNCTUATE-Contraction:  ~s ~s ~s~%" x y z)
             (make-contraction x y z))
         ;; Default: put a space between words.
         (t
            ;(format t "PUNCTUATE:  Fired Default Rule: ~s ~s ~s~%" x y z)
            (list x " "))))
    (butlast sentence)
    (cdr sentence) (append (cddr sentence) '("")))
       (last sentence)))))


;; CBC - Version 2.3: Not very efficient, but determines contractions
(defun make-contraction (z y three)
  (setf *contraction-delete* t)
  (let* ((x (string-downcase z))  (c  (equal three ","))
         (word
  (cond ((and (equal x "i") (equal y "am"))       (list "I'm" " "))
        ((and (equal x "you") (equal y "are"))    (list "you're" " "))
        ((and (equal x "he") (equal y "is"))      (list "he's" " "))
        ((and (equal x "she") (equal y "is"))     (list "she's" " "))
        ((and (equal x "it") (equal y "is") (not c))      (list "it's" " "))
        ((and (equal x "we") (equal y "are"))     (list "we're" " "))
        ((and (equal x "they") (equal y "are"))   (list "they're" " "))
        ((and (equal x "there") (equal y "is"))   (list "there's" " "))
        ((and (equal x "here") (equal y "is"))    (list "here's" " "))
        ((and (equal x "how") (equal y "is"))     (list "how's" " "))
        ((and (equal x "is") (equal y "not"))     (list "isn't" " "))
        ((and (equal x "are") (equal y "not"))    (list "aren't" " "))
        ((and (equal x "was") (equal y "not"))    (list "wasn't" " "))
        ((and (equal x "were") (equal y "not"))   (list "weren't" " "))
        ((and (equal x "have") (equal y "not"))   (list "haven't" " "))
        ((and (equal x "has") (equal y "not"))    (list "hasn't" " "))
        ((and (equal x "had") (equal y "not"))    (list "hadn't" " "))
        ((and (equal x "do") (equal y "not"))     (list "don't" " "))
        ((and (equal x "does") (equal y "not"))   (list "doesn't" " "))
        ((and (equal x "did") (equal y "not"))    (list "didn't" " "))
        ((and (equal x "will") (equal y "not"))   (list "won't" " "))
        ((and (equal x "can") (equal y "not"))    (list "can't" " "))
        ((and (equal x "would") (equal y "not"))  (list "wouldn't" " "))
        ((and (equal x "should") (equal y "not")) (list "shouldn't" " "))
        ((and (equal x "could") (equal y "not"))  (list "couldn't" " "))
        ((and (equal x "might") (equal y "not"))  (list "mightn't" " "))
        ((and (equal x "need") (equal y "not"))   (list "needn't" " "))
        ((and (equal x "i") (equal y "have"))     (list "I've" " "))
        ((and (equal x "he") (equal y "has"))     (list "he's" " "))
        ((and (equal x "she") (equal y "has"))    (list "she's" " "))
        ((and (equal x "it") (equal y "has"))     (list "it's" " "))
        ((and (equal x "you") (equal y "have"))   (list "you've" " "))
        ((and (equal x "we") (equal y "have"))    (list "we've" " "))
        ((and (equal x "they") (equal y "have"))  (list "they've" " "))
        ((and (equal x "i") (equal y "had"))      (list "I'd" " "))
        ((and (equal x "he") (equal y "had"))     (list "he'd" " "))
        ((and (equal x "she") (equal y "had"))    (list "she'd" " "))
        ((and (equal x "it") (equal y "had"))     (list "it'd" " "))
        ((and (equal x "you") (equal y "had"))    (list "you'd" " "))
        ((and (equal x "we") (equal y "had"))     (list "we'd" " "))
        ((and (equal x "they") (equal y "had"))   (list "they'd" " "))
        ((and (equal x "i") (equal y "will"))     (list "I'll" " "))
        ((and (equal x "he") (equal y "will"))    (list "he'll" " "))
        ((and (equal x "she") (equal y "will"))   (list "she'll" " "))
        ((and (equal x "it") (equal y "will"))    (list "it'll" " "))
        ((and (equal x "you") (equal y "will"))   (list "you'll" " "))
        ((and (equal x "we") (equal y "will"))    (list "we'll" " "))
        ((and (equal x "they") (equal y "will"))  (list "they'll" " "))
        ((and (equal x "i") (equal y "would"))    (list "I'd" " "))
        ((and (equal x "he") (equal y "would"))
           (if (equal three "not")  (progn (list x " " "wouldn't") (setf *contraction-delete* nil))   (list "he'd" " ")))
        ((and (equal x "she") (equal y "would"))  (list "she'd" " "))
        ((and (equal x "it") (equal y "would"))   (list "it'd" " "))
        ((and (equal x "you") (equal y "would"))  (list "you'd" " "))
        ((and (equal x "we") (equal y "would"))   (list "we'd" " "))
        ((and (equal x "they") (equal y "would")) (list "they'd" " "))
        ((and (equal x "where") (equal y "is"))   (list "where's" " "))
        ((and (equal x "there") (equal y "is"))   (list "there's" " "))
        ((and (equal x "there") (equal y "will")) (list "there'll" " "))
        ((and (equal x "that") (equal y "is"))    (list "that's" " "))
        ((and (equal x "that") (equal y "will"))  (list "that'll" " "))
        ((and (equal x "what") (equal y "is"))    (list "what's" " "))
        ((and (equal x "who") (equal y "is"))     (list "who's" " "))
        ((and (equal x "now") (equal y "is"))     (list "now's" " "))
        ((and (equal x "let") (equal y "us"))     (list "let's" " "))
        ((and (equal x "everybody") (equal y "is"))  (list "everybody's" " "))
        (t    (setf *contraction-delete* nil) (list x " ")))))
  (if (not (equal x z)) (setf word (list (string-upcase (first word) :end 1) (second word))))
  ;(format t "Contraction:  ~S,  ~S,  ~S,  ~S~%" x y three word)
  (if *contraction-delete*
      word
      (list z " "))))
     ;(if (equal x z) x (string-upcase x :end 1)))))



;; ------------------------------------------------------------
;; LINEARIZER
;; ------------------------------------------------------------

;; If your system already has string-append defined,
;; Comment out the following or add EXCL to package imports:
(defmacro string-append (&rest strings)
  "Append s1 to s2"
  `(concatenate 'string ,@strings))

(defun linearize (fd path &key (cat-attribute *cat-attribute*))
  "Linearizes fd (found at level path from the root of the total
       fd) and returns a list of strings.
       Handles gaps and punctuation.
       If a feature (gap xxx) where xxx is not none is found in a
       constituent, the constituent does not appear in the result.
       The features punctuation have a special effect described
       in documentation of function list-punctuation."
  (cond
    ((leaf-p fd) nil)
    (t
     (let* ((pattern     (incr-gdp 'pattern fd path))
        (gap         (incr-gdp 'gap fd path))
        (punctuation (incr-gdp 'punctuation fd path))
        (punct-path  (path-extend path 'punctuation))
        (lin-const
          (cond
            ((and gap (not (eq gap 'none)))
            ;; constituent is gapped - don't put it in linearized
              nil)
            (pattern  ;; recurse on all constituents
              (let ((order (clean-pattern pattern)))
                (list-punctuation
              punctuation punct-path
              (mapcan
                #'(lambda (constituent)
                  (let ((new-path (safe-path-extend path constituent))
                    (new-fd
                      (if (attr-p constituent)
                        (incr-gdp constituent fd path)
                        (gdp *input*
                         (absolute-path
                           constituent
                           (path-extend path 'pattern))))))
                    (linearize new-fd new-path)))
              order))))
            (t  ;; simple constituent: call morphology
               (let ((cat (incr-gdp cat-attribute fd path))
                 (lex (incr-gdp 'lex fd path)))
                 (list-punctuation punctuation punct-path
                                       (morph cat lex fd path
                       :cat-attribute cat-attribute))))))
        (html        (incr-gdp 'html fd path))
        (hpath       (path-extend path 'html))
            (latex       (incr-gdp 'latex fd path))
            (lpath       (path-extend path 'latex))
            (mathmode    (incr-gdp 'mathmode fd path))
            (premath
    (cond ((and html
                (not (eq html 'none)))
                  ;; CBC - Version 2.3: Let punctuation/capitalization know we're in html mode
                  (setf *html-found* t)
                  (cons (html-tag html hpath :start)
                        (append  lin-const
                                 (list (html-tag html hpath :end)))))
              ((and latex
                    (not (eq latex 'none)))
                  (cons (latex-tag latex lpath :start)
                        (append  lin-const
                                 (list (latex-tag latex lpath :end)))))
              (t   lin-const))))
         (if mathmode (append '("$") premath '("$"))  premath)))))

;; hpath is the path to the html fd within the total fd (to dereference paths
;; when needed)
;; html-fd is an fd with pairs as follows:
;; (<tag> +) where tag can be any string or
;; (<tag> ((<att> <val>) ...)) for html tags with attributes
;; The following special attributes are defined:
;; html-alt: instead of alt (in IMG) to avoid conflict with alt.
;; start-tag: if the HTML tag is to be different from <tag>, give
;;            its value as a string without the <>.
;; end-tag: if the end tag is different from </tag>
;;          set it to none if no end tag is to be used.
;;          eg <a name=index1> or <img src=a.gif>
;; mode is either :start or :end
;; Return: a string with the opening or closing html tag.
(defun html-tag (html-fd hpath mode)
  (let ((res ""))
    (mapc #'(lambda (pair)
              (let* ((vpath (safe-path-extend hpath (car pair)))
                     (att (if (symbolp (car pair))
                              (car pair)
                            (path-last vpath)))
                     (val (incr-gdp (car pair) html-fd hpath)))
                (if (eq mode :start)
                    ;; CBC - Version 2.3: HTML tags should be downcased
                    (setf res (concatenate
                               'string
                               res
                               (html-tag-att att val vpath mode)))
                    (setf res (concatenate
                               'string
                               (html-tag-att att val vpath mode)
                               res)))))
          html-fd)
    res))

;; CBC - Version 2.3: Reorganized this function, especially for (val -)
(defun html-tag-att (att val vpath mode)
  (case mode
   (:start
    (let ((fd-tag (incr-gdp 'start-tag val vpath)))
      (unless (and fd-tag (not (eq fd-tag 'none)))
              (setf fd-tag att))
      (if (symbolp fd-tag) (setf fd-tag (string-downcase (symbol-name fd-tag))))
      ;; Translate all pairs into att=val strings
      ;; If val is just + just return ""
      (if (and (not (symbolp val))
               (equal (incr-gdp 'start-tag val vpath) 'none))
          ""
          (cond ((eq val '+)
                    (string-append "<" fd-tag (html-tag-att-format val vpath) ">"))
                ((eq val '-)
                    (string-append "</" fd-tag (html-tag-att-format val vpath) ">"))
                (t  (string-append "<" fd-tag (html-tag-att-format val vpath) ">"))))))
   (:end
    (cond ((eq val '+)
              (string-append "</" (string-downcase (symbol-name att)) ">"))
          ((eq val '-)
              (string-append "<" (string-downcase (symbol-name att)) ">"))
          (t  (let ((fd-tag (incr-gdp 'end-tag val vpath)))
                (if (eq fd-tag 'none)
                    ""
                    (let ((fd-tag (if (null fd-tag) att fd-tag)))
                      (if (symbolp fd-tag)
                          (setf fd-tag (string-downcase (symbol-name fd-tag))))
                      (string-append "</" fd-tag ">")))))))))

(defun html-tag-att-format (val vpath)
  (cond ((eq val '+)  "")
        ((eq val '-)  "")
    (t  (let ((res ""))
          (mapc #'(lambda (pair)
                    (let ((modif (car pair)))
                    (cond ((not (symbolp modif)))
                          ((eq modif 'start-tag))
                          ((eq modif 'end-tag))
                          (t
                           (let ((modif-val (incr-gdp modif val vpath)))
                             (if (eq modif 'html-alt) (setf modif 'alt))
                             (if (or (numberp modif-val)
                                     (stringp modif-val)
                                     (symbolp modif-val))
                                 (setf res (format nil "~a ~a=~s"
                                                   res (string-downcase (symbol-name modif))
                                                       modif-val))))))))
                val)
          res))))

;; CBC - Version 2.3: Added LaTeX functions
(defun latex-tag (latex-fd lpath mode)
  (let ((res ""))
    (mapc #'(lambda (pair)
              (let* ((vpath (safe-path-extend lpath (car pair)))
                     (att (if (symbolp (car pair))
                              (car pair)
                            (path-last vpath)))
                     (val (incr-gdp (car pair) latex-fd lpath))
                     (prebox  (incr-gdp 'prebox latex-fd vpath))
                     (postbox (incr-gdp 'postbox latex-fd vpath))
                     (nospace (incr-gdp 'nospace latex-fd vpath)))
                (if (eq mode :start)
                    (setf res (concatenate
                               'string res
                               (latex-tag-att att val vpath mode prebox postbox
                                              nospace)))
                    (setf res (concatenate
                               'string
                               (latex-tag-att att val vpath mode prebox postbox)
                               res)))))
          latex-fd)
    res))

(defun latex-tag-att (att val vpath mode &optional (prebox nil) (postbox nil)
                                                   (nospace nil))
  (case mode
   (:start
    (let ((fd-tag (incr-gdp 'start-tag val vpath))
          (tilde  (if nospace "~~"  ""))
          (pbtext (if prebox (string-append "[" (latex-arglist prebox) "]")  "")))
      (unless (and fd-tag (not (eq fd-tag 'none)))
              (setf fd-tag att))
      (if (symbolp fd-tag) (setf fd-tag (symbol-name fd-tag)))
      (cond ((or (equal att 'prebox) (equal att 'postbox) (equal att 'nospace)) "")
            ((equal att 'latex-linebreak) "\\\\")
            ((or (eq val '+) (eq val '/))
                 (string-append "{\\" (string-downcase fd-tag) " "))
            ((equal val '-)
                 (string-append tilde "\\" (string-downcase fd-tag) " "))
            ((equal att 'latex-brace)
                 (string-append "\\" (string-downcase (string val)) "{"))
            ((equal att 'latex-bracket)
                 (string-append "\\" (string-downcase (string val)) "["))
            ((stringp val)
                 (string-append tilde "\\" (string-downcase fd-tag) pbtext "{" val))
;            ((and val (listp val))
;                 (string-append "\\" (string-downcase (string (caar val)))
;                    #\{ (string-downcase (string (cadar val)))
;                    (if (rest val)
;                    (string-append " " (latex-tag-att att (rest val) vpath mode)) "")))
            (t   (format t "Error in latex-tag-att.~%")))))
   (:end
    (cond ((or (equal att 'latex-linebreak) (equal att 'prebox) (equal att 'postbox)
               (equal att 'nospace))   "")
          ((eq val '+) (string "}"))
          ((eq val '/) "\\/}")
          (t  (let ((fd-tag (incr-gdp 'end-tag val vpath)))
                (if (and (eq fd-tag 'none) (equal val '-))
                    ""
                    (string-append (if (equal att 'latex-bracket)
                                       (string "]")  (string "}"))
                                   (if postbox (string-append "["
                                                  (latex-arglist postbox) "]") "")
                                   ))))))))


(defun latex-arglist (val)
  (if (rest val)
      (string-append (string-downcase (string (caar val)))
                     "," (latex-arglist (rest val)))
      (string-downcase (string (caar val)))))


;; ------------------------------------------------------------
;; MORPHOLOGY
;; From a draft by Jay Meyer (USC/ISI).
;; Changed by Michael Elhadad.
;; ------------------------------------------------------------
(defun list-if-atom (item)
  "Returns the list of an atom when passed an atom other than nil."
  (if (listp item) item (list item)))

(defun MORPH (cat lex fd path &key (cat-attribute *cat-attribute*) &aux (orig-cat))
  "Dispatch according to cat to specialized morphology routine.
       Handles verbs, nouns, pronouns and dets."
  (declare (special *unknown-cat*))
  (unless (stringp lex)
    (setq lex (string-downcase (format nil "~s" lex))))

  ;; Normalize subsumed categories to their normal representative
  (let ((normal-cat (member-if #'(lambda (c) (subsume c cat))
                   '(adj adv conj modal prep relpro punctuation
                     phrase noun verb pronoun article ordinal
                     cardinal fraction))))
    (setf orig-cat cat)
    (if normal-cat
        (setf cat (car normal-cat))))

  (list-if-atom
   (case cat
     ;; CBC - Version 2.3: Removed adverb and adjective
     ((conj modal prep relpro interjection punctuation phrase) lex)
     ;; CBC - Version 2.3: Adjectives need polarity
     (adv (morph-adv lex
             (check-polarity (incr-gdp 'polarity fd path))))
     ;; CBC - Version 2.3: New features for adjectives
     (adj (morph-adj lex
             (check-superlative (incr-gdp 'superlative fd path))
             (check-comparative (incr-gdp 'comparative fd path))
             (check-inflection (incr-gdp 'inflected fd path))
             (check-degree (incr-gdp 'degree fd path))
             (check-orientation (incr-gdp 'orientation fd path))))
     (noun (morph-noun
        lex
        (incr-gdp 'number fd path)
        (check-a-an (incr-gdp 'a-an   fd path))
        (incr-gdp 'feature fd path)
        ;; CBC - Version 2.3: Added acronyms (mainly for pluralization)
        (incr-gdp 'acronym fd path)))
     (verb (morph-verb
        lex
        (incr-gdp 'ending fd path)
        (check-number (incr-gdp 'number fd path))
        (check-person (incr-gdp 'person fd path))
        (check-tense (incr-gdp 'tense fd path))
        ;; CBC - Version 2.3: The feature for verbs like "get going"
        (check-gap-inf (incr-gdp 'gapped-infinitive fd path))))
     (pronoun
      (morph-pronoun
       lex
       (or (incr-gdp 'pronoun-type fd path) cat)
       (check-case (incr-gdp 'case fd path))
       (check-gender (incr-gdp 'gender fd path))
       (check-number (incr-gdp 'number fd path))
       (check-distance (incr-gdp 'distance fd path))
       (check-animate (incr-gdp 'animate fd path))
       (check-person (incr-gdp 'person fd path))
       ;; CBC - Version 2.3: Added new pronoun features
       (check-restrictive (incr-gdp 'restrictive fd path))
       (check-anaphor (incr-gdp 'anaphoric fd path))
       (check-own (incr-gdp 'own fd path))
       (check-formality (incr-gdp 'formality fd path))
       orig-cat))
     ((article det)
      (morph-determiner
       lex
       (incr-gdp 'number fd path)))
     ((ordinal cardinal)
      (morph-numeric lex cat
             (incr-gdp 'value fd path)
             (incr-gdp 'digit fd path)
             ;; CBC - Version 2.3: These are due mainly to Penn TreeBank
             ;;                    financially-related numbers
             (incr-gdp 'percent fd path)
             (incr-gdp 'prefix fd path)
             (incr-gdp 'modifier fd path)
             (incr-gdp 'format fd path)
             (incr-gdp 'number-units fd path)))
     (fraction
      (morph-fraction lex
              (incr-gdp 'num fd path)
              (incr-gdp 'den fd path)
              (incr-gdp 'digit fd path)))
     ;; CBC - Version 2.3: New category, also due to PTB
     (numeric-range
      (morph-range lex
              (incr-gdp 'low fd path)
              (incr-gdp 'high fd path)
              (incr-gdp 'dash fd path)
              (incr-gdp 'digit fd path)))
     (t (cond ((member cat (categories-not-unified cat-attribute)) lex)
          (t
           (push cat *unknown-cat*)
           (format nil "<unknown cat ~s: ~a>" cat lex)))))))

;; -----------------------------------------------------------------------
;; MORPHOLOGY-HELP
;; -----------------------------------------------------------------------
(defun morphology-help (&optional unknown-cats)
  "Gives feedback to users who have done something wrong with morpho."
  (when unknown-cats
    (format
     t
     "You have used the following categories which are not known ~@
      by the morphology module: ~s~%" unknown-cats))
  (format
   t
   "The categories known by the morphology module are: ~@
    adv, conj, modal, prep, relpro, punctuation, phrase:
              lex is sent unmodified.~@
    adj:          inflection on superlative, comparative, inflection.~@
    noun:         agreement done on number, a-an and feature.~@
    verb:         agreement done on ending, number, person and tense.~@
    pronoun:      agreement done on pronoun-type, case, gender, number,
              distance, animate and person.~@
    det, article: agreement done on number. A/an is processed.~@
    fraction: find string based on num, den (numbers) and digit (yes, roman or no).~@
    ordinal, cardinal: find string based on value (a number) and
              digit (yes, roman or no) to use letters or digits.~%")
  (values))


;; ------------------------------------------------------------
;; Checks: verify validity of features influencing morphology
;; and chooses default values.
;; ------------------------------------------------------------
(defun CHECK-PERSON (person)
  "Returns person, DEFAULT is THIRD."
  (cond ((member person '(first second third)) person)
        (t 'third)))

(defun CHECK-TENSE (tense)
  "Returns tense, DEFAULT is PRESENT."
  (cond ((member tense '(present past)) tense)
    (t 'present)))

(defun CHECK-NUMBER (number)
  "Returns number, DEFAULT is SINGULAR."
  (cond ((member number '(plural dual not-one)) number)
        (t 'singular)))

(defun CHECK-CASE (case)
  "Returns case, DEFAULT is SUBJECTIVE."
  (cond ((member case '(subjective objective possessive reflexive)) case)
    (t 'subjective)))

(defun CHECK-GENDER (gender)
  "Return gender, DEFAULT is MASCULINE."
  (cond ((member gender '(masculine feminine neutral)) gender)
    (t  'masculine)))

(defun CHECK-GAP-INF (gap-inf)
  "Return anaphor, DEFAULT is NO."
  (cond ((member gap-inf '(yes no)) gap-inf)
    (t  'no)))

(defun CHECK-DISTANCE (distance)
  "Return distance, DEFAULT is NEAR."
  (cond ((member distance '(near far)) distance)
    (t  'near)))

(defun CHECK-ANIMATE (animate)
  "Return animate, DEFAULT is NO."
  (cond ((member animate '(yes no)) animate)
    (t 'no)))

(defun CHECK-A-AN (a-an)
  "Return feature A-AN, DEFAULT is CONSONANT."
  (cond ((null a-an) 'consonant)
    (t a-an)))

(defun CHECK-HTML (html)
  "Return feature HTML, DEFAULT is NONE."
  (cond ((null html) 'none)
    (t html)))

;; CBC - Version 2.3: Checks on the values of the new features

(defun CHECK-ANAPHOR (anaphor)
  "Return anaphor, DEFAULT is NO."
  (cond ((member anaphor '(yes no)) anaphor)
    (t  'no)))

(defun CHECK-LATEX (latex)
  "Return feature LATEX, DEFAULT is NONE."
  (cond ((null latex) 'none)
    (t latex)))

(defun CHECK-RESTRICTIVE (restrictive)
  "Return restrictive, DEFAULT is NO"
  (cond ((member restrictive '(yes no)) restrictive)
    (t 'no)))

(defun CHECK-POLARITY (polarity)
  "Return polarity, DEFAULT is POSITIVE"
  (cond ((member polarity '(positive negative)) polarity)
    (t 'positive)))

;; yd add check superlatives..
(defun CHECK-SUPERLATIVE (superlative)
  (cond ((member superlative '(yes)) 'superlative)
        (t 'no)))

(defun CHECK-COMPARATIVE (comparative)
  (cond ((member comparative '(yes)) 'comparative)
        (t 'no)))

(defun CHECK-INFLECTION (inflection)
  (if (eq inflection 'yes) inflection 'no))

(defun CHECK-DEGREE (degree)
  (cond ((eq degree '+) '+)
        ((eq degree '-) '-)
        (t  'none)))

(defun CHECK-ORIENTATION (orientation)
  (cond ((eq orientation '+) '+)
        ((eq orientation '-) '-)
        (t  'none)))

(defun CHECK-FORMALITY (formality)
  (cond ((eq formality 'informal) 'informal)
        (t  'formal)))

(defun CHECK-OWN (own)
  (cond ((eq own 'own) 'own)
        (t  'none)))



;; ------------------------------------------------------------
;; Specialized Morph. Routines by Jay Meyer (USC/ISI) and Michael Elhadad
;; ------------------------------------------------------------

(defun MORPH-NOUN (word number a-an feature acronym)
  "If feature is possessive, then return the apostrephised form of the noun
   appropriate to the number.
   If a-an is 'an mark noun with mark-noun-as-an to agree with determiner.
   If noun is plural and ends with 's', mark noun as plural to let it agree
   with possessive mark that may follow it (single apostrophe or 's).
   Return word with number suffix."
  ;; CBC - Version 2.3: Added acronym support, removed possessive from here
  (setq word (morph-number word number acronym))
  (if (and (subsume 'plural number)
           (< 1 (length word))
           (eql (elt word (1- (length word))) #\s))
      (setf word (mark-noun-as-plural word acronym)))
  (if (eql a-an 'an) (mark-noun-as-an word) word))


;; Utils to deal with a-an feature. A-an marks are dealt with in punctuate.
(defun mark-noun-as-an (word)
  (concatenate 'string *a-an-marker* word))

(defun unmark-noun-as-an (word)
  (subseq word (length *a-an-marker*)))

(defun noun-marked-as-an (word)
  (and (> (length word) (length *a-an-marker*))
       (string= (subseq word 0 (length *a-an-marker*)) *a-an-marker*)))


;; CBC - Version 2.3: Added acronym support
(defun MORPH-NUMBER (word number acronym)
  "Adds the plural suffix to word if number is plural
   note that the default is singular."
  (cond ((null number) word)
    ((null word) word)
        ((subsume 'plural number)
         (cond ((lexfetch word 'plural))
               ((lexfetch (string-downcase word) 'plural))
               (t (pluralize word acronym))))
        (t word)))


(defun MORPH-VERB (word ending number person tense gapped-infinitive)
  "Adds the proper suffix to the verb root taking into account
   ending, number, person, and tense."
  (cond ((equal word "") (format t "Linearize verb error: empty verb string~%") "verb")
        ;; CBC - Version 2.3: Allow new verb types
        ((equal word "verb-gap") "")
        ((eq ending 'root) word)
        ((eq gapped-infinitive 'yes) word)
        ;; CBC - Version 2.3: Eventually will need a new treatment, as this
        ;;                    method will not allow for split infinitives
        ((eq ending 'infinitive) (list "to" word))
        ((equal word "be") (morph-be number person (or ending tense)))
        ((eq ending 'present-participle)
         (cond ((lexfetch word 'present-participle))
               (t (form-ing word))))
        ((eq ending 'past-participle)
         (cond ((lexfetch word 'past-participle))
               (t (form-past word))))
        ((lexfetch word (if (and (eq tense 'present)
                            (eq person 'third)
                            (eq number 'singular))
                       'present-third-person-singular
                       tense)))
        ((eq tense 'present) (form-present-verb word number person))
        ((eq tense 'past) (form-past word))
        (t nil)))


(defun MORPH-BE (number person tense)
  (case tense
    (present (case number
           (singular (case person
               (first "am")
               (second "are")
               (third "is")))
           (t  "are")))
    (past (case number
        (singular (case person
            (first "was")
            (second "were")
            (third "was")))
        (t "were")))
    (present-participle "being")
    (past-participle "been")))


;;; YD add morphology for the superlative/comparative inflections

(defun MORPH-ADJ (word2 superlative comparative inflection degree orientation)
  (let* ((word    (if (equal word2 "nil") "" word2))
         (ending  (if (eq superlative 'no)
                      (if (eq comparative 'no) '() 'comparative)
                      'superlative))
         (content
      (cond ((null ending)  word)
            ((lexfetch word ending))
            ((and (eq ending 'comparative) (eq inflection 'no) (eq orientation '-))
                (if (equal word "") "less" (string-append "less " word)))
            ((and (eq ending 'comparative) (eq inflection 'no))
                (if (equal word "") "more" (string-append "more " word)))
            ((and (eq ending 'superlative) (eq inflection 'no) (eq orientation '-))
                (if (equal word "") "least" (string-append "least " word)))
            ((and (eq ending 'superlative) (eq inflection 'no))
                (if (equal word "") "most" (string-append "most " word)))
            ((eq ending 'superlative)
               (if (lexfetch word 'superlative-adjective)
                   (lexfetch word 'superlative-adjective)
                   (form-adj word "est")))
            (t (if (lexfetch word 'comparative-adjective)
                   (lexfetch word 'comparative-adjective)
                   (form-adj word "er"))))))
      (cond ((eq degree '+)  (string-append "even " content))
            (t  content))))

(defun FORM-ADJ (word ending)
  "changes in spelling:
   1. final base consonants are doubled when preceding vowel is stressed and
      spelled with a single letter: big-bigger-biggest; sad-sadder-saddest.
   2. bases ending in a consonant+y, final y changed to i
      angry-angrier-angriest.
   3. base ends in a mute -e, dropped before inflectional suffix:
      pure-purer-purest
      free-freer-freest"
   (let ((final (last-char word)))
     (case final
             (#\e (concatenate 'string (butlast-char word) ending))
             ((#\b #\d #\g #\m #\n #\p #\t)
              (if (and (vowel (elt word (- (length word) 2)))
               (not (vowel (elt word (- (length word) 3)))))
              (concatenate 'string word (list final) ending)
            (concatenate 'string word ending)))
             (#\y (if (vowel (elt word (- (length word) 2)))
                      (concatenate 'string word ending)
                    (concatenate 'string (butlast-char word) "i" ending)))
             (t (concatenate 'string word ending)))))


(defun FORM-PRESENT-VERB (word number person)
  "Forms the suffix for the present tense of the verb WORD."
  (when word
    (case person
      (first word)
      (second word)
      (third (case number
           (singular (pluralize word nil))
           (plural word)
           (dual word)
           (not-one word)
           (otherwise nil)))
      (otherwise nil))))


;; CBC - Version 2.3: Added a large number of default cases
(defun FORM-PAST (word)
  "Form past tense by adding 'ed to word,
  handles duplication of final consonant and special cases. "
  (let ((char  (elt word (1- (length word))))
        (char2 (if (< 1 (length word)) (elt word (- (length word) 2))))
        (char3 (if (< 2 (length word)) (elt word (- (length word) 3)))))
    (case char
      (#\e (concatenate 'string word "d"))
      (#\y (if (vowel (elt word (- (length word) 2)))
           (concatenate 'string word "ed")
         (concatenate 'string (subseq word 0 (1- (length word)))
              "ied")))
      (#\r (cond ((and (equal char2 #\a) (not (equal char3 #\e)))
                     (concatenate 'string word (list char) "ed"))
                 (t  (concatenate 'string word "ed"))))
      ((#\b #\d #\g #\m #\n #\p #\t)
        (cond ((and (equal char #\n) (equal char2 #\e)) ;; "listen" --> "listened"
                  (concatenate 'string word "ed"))
              ((and (equal char #\m) (equal char2 #\o)) ;; "bottom" --> "bottomed"
                  (concatenate 'string word "ed"))
              ((and (equal char #\t) (or (equal char2 #\e))) ;; "rivet" --> "riveted"
                  (concatenate 'string word "ed"))
              ((and (equal char #\t) (equal char2 #\i)
                    (or (equal char3 #\d) (equal char3 #\x) (equal char3 #\r))) ;; "credit" --> "credited" exited inherited  #submitted
                  (concatenate 'string word "ed"))
              ((and (vowel char2) (not (vowel (elt word (- (length word) 3)))))
                  (concatenate 'string word (list char) "ed"))
              (t  (concatenate 'string word "ed"))))
      (#\l (if (and (equal char2 #\e) (equal char3 #\p)) ;; "compel", "propel"
               (concatenate 'string word "led")
               (concatenate 'string word "ed")))
      (t (concatenate 'string word "ed")))))


;; CBC - Version 2.3: Added a large number of default cases
(defun FORM-ING (word)
  "Add 'ing to word dropping final 'e if any,
  handles duplication of final consonant and special cases."
  (let ((char (elt word (1- (length word)))))
    (cond ((equal (last3-str word) "ten")   (string-append word "ing"))
          ((equal (last3-str word) "ket")   (string-append word "ing"))
          ((equal (last3-str word) "sen")   (string-append word "ing"))
          ((equal (last4-str word) "efer")  (string-append word "ring"))
          ((equal (last3-str word) "cit")   (string-append word "ing"))
          ((equal (last3-str word) "son")   (string-append word "ing"))
          ((equal (last2-str word) "ee")    (string-append word "ing"))   ;; "agree"
          ((equal (last2-str word) "ie")    (string-append (subseq word 0 (- (length word) 2)) "ying"))   ;; "die"
      (t  (case char
             (#\e (concatenate 'string (subseq word 0 (1- (length word))) "ing"))
             (#\r (if (and (char-equal (elt word (- (length word) 2)) #\a)
                           (not (char-equal (elt word (- (length word) 3)) #\e)))
                      (concatenate 'string word (list char) "ing")
                      (concatenate 'string word "ing")))
             ;(#\n (if (and (member2 (elt word (- (length word) 2)) '(#\e))
             ;              (vowel (elt word (- (length word) 2))))
             ;         (concatenate 'string word "ing")
             ;         (concatenate 'string word (list char) "ing")))
             ((#\b #\d #\g #\m #\p #\t #\n)
                  (if (and (vowel (elt word (- (length word) 2)))
                           (or (= 2 (length word))
                               (not (vowel (elt word (- (length word) 3))))))
                      (concatenate 'string word (list char) "ing")
                      (concatenate 'string word "ing")))
             (t (concatenate 'string word "ing")))))))


(defun MORPH-DETERMINER (word number)
  "Returns the correct article for the given type.
       DOES NOT CHECK for next word after indefinite - this is
       done in PUNCTUATE, but here just put special mark
       *indefinite-article* to indicate something is to be done."
  (cond ((equal word "a")
         (cond ((subsume 'plural (check-number number)) nil)
           (t *indefinite-article*)))
        (t word)))


;; CBC - Version 2.3: Put negation morphology on the adverb itself
(defun MORPH-ADV (word polarity)
  (cond ((equal polarity 'negative)
            (string-append "not " word))
        (t  word)))


;; CBC - Version 2.3: Handle new features:  restrictive, anaphoric, own, formality
;;                    Formality is more important in non-English languages (but test here for "'em")
;;                    Anaphora should work for one-anaphora
(defun MORPH-PRONOUN (lex pronoun-type case gender number
              distance animate person restrictive anaphoric own formality orig-cat)
  "Returns the correct pronoun given the features."
  (if (null pronoun-type) (setf pronoun-type 'personal))
  (if (equal orig-cat 'relative-pronoun) (setf pronoun-type 'relative))
  (cond
    ((equal own 'own) (string-append (morph-pronoun lex pronoun-type case gender
             number distance animate person restrictive anaphoric nil formality) " own"))
    ((and (stringp lex)
          (not (string= lex "nil"))
          (not (string= lex "none"))) lex)
    ((and (equal anaphoric 'yes) (equal pronoun-type 'personal))
     (if (eq number 'plural) "ones" "one"))
    ((eq pronoun-type 'personal)
     ;; start with "he" then augment by person, then by
     ;; number, gender and finally by case.
     (when (eq animate 'no) (setq gender 'neuter))
     (setq lex "he")
     (unless (eq person 'third)
       (setq lex (cond ((lexfetch lex person)) (t lex))))
     (unless (eq number 'singular)
       (setq lex (cond ((lexfetch lex number)) (t lex))))
     (unless (eq gender 'masculine)
       (setq lex (cond ((lexfetch lex gender)) (t lex))))
     (unless (eq case 'subjective)
       (setq lex (cond ((lexfetch lex case)) (t lex))))
     (if (and (equal lex "them") (equal formality 'informal)) (setq lex "'em"))
     lex)
    ((eq pronoun-type 'demonstrative)
     (cond ((subsume 'plural number)
        (if (eq distance 'far) "those" "these"))
           ((eq distance 'far) "that")
           (t "this")))
    ((eq pronoun-type 'relative)
     (when (eq animate 'no) (setq gender 'neuter))
     (cond ((eq restrictive 'yes) "that") ;(if (eq animate 'no) "that" "who"))
           ((eq case 'possessive) "whose")
           ((eq case 'objective)
        (if (eq gender 'neuter) "which" "whom"))
           ((eq gender 'neuter) "which")
           ((eq animate 'no) "which")
           (t "who")))
    ((eq pronoun-type 'question)
     (when (eq animate 'no) (setq gender 'neuter))
     (cond ((eq restrictive 'yes) "which")
           ((eq case 'possessive) "whose")
           ((eq case 'objective)
        (if (eq gender 'neuter) "what" "whom"))
           ((eq gender 'neuter) "what")
           ((eq animate 'no) "what")
           (t "who")))
    ((eq pronoun-type 'quantified)
     lex)))


(defun format-ordinal (value)
  ;; Integer to string of the form "nth"
  ;; 1 -> st except for 11
  ;; 2 -> nd except for 12
  ;; 3 -> rd except for 13
  ;; otw th
  (let* ((lasttwo (mod value 100))
     (lastone (mod value 10))
     (ext (cond ((member lasttwo '(11 12 13)) "th")
            ((= lastone 1) "st")
            ((= lastone 2) "nd")
            ((= lastone 3) "rd")
            (t "th"))))
    (format nil "~s~a" value ext)))


;; CBC - Version 2.3: Add a number of new features for cardinals/ordinals

(defun MORPH-NUMERIC (lex ord-or-card value digit percent prefix mod frmt units)
  (let ((num (morph-numeric-aux lex ord-or-card value digit prefix mod frmt units)))
    (if (and percent (not (equal percent 'none)))
        (string-append num "%")
        num)))

(defun MORPH-NUMERIC-AUX (lex ord-or-card value digit prefix mod frmt units)
  "Return a string given a number - either ordinal or cardinal"
  ;; Lucky common-lisp does all for us!!!
  (if (and mod (not (stringp mod))) (setq mod (format nil "~S" mod)))
  (let* ((str
  (cond ((and (stringp lex)
              (not (numberp value))     ;; if value is given use it
              (not (string= lex "nil"))
              (not (string= lex "none"))) lex)
        ((integerp value)
           (if (and (> value 9999) frmt (not (equal frmt 'none)))
               (format nil "~:D" value)
               (case digit
                  (no (case ord-or-card
                            (ordinal (format nil "~:r" value))
                            (cardinal (format nil "~r" value))
                            (t  (format nil "~r" value))))
                  (roman (format nil "~@R" value))
                  (t  (case ord-or-card
                            (ordinal (format-ordinal value))
                            ;; CBC - Version 2.3: No commas for years, but this is a bad way to do it
                            (t  (if (or (null frmt) (equal frmt 'none) (equal frmt 'year))
                                (format nil "~D" value)
                                (format nil "~:D" value))))))))
        ((numberp value)  (format nil "~s" value))
        ((stringp value) value)
        (t  (format nil "<unknown-number: ~s>" value))))
      (str2  (if (stringp prefix)  (string-append prefix str)  str))
      (str3  (if (stringp units)   (string-append str2 " " units)  str2))
      (str4  (if (and (equal str3 "one hundred") mod) "hundred" str3)))
  (if mod (string-append mod " " str4) str4)))


(defun MORPH-FRACTION (lex num den digit)
  "Return a string for a fraction"
  (cond ((and (integerp num) (integerp den))
     (cond ((= den 1)
        (case digit
          (no (format nil "~r" num))
          (roman (format nil "~@R" num))
          (t (format nil "~s" num))))
           ((and (= num 1) (= den 2))
        (case digit
          (no "half")
          (t "1/2")))
           ((= den 2)
        (case digit
          (no "~r halves" num)
          (t "~s/2" num)))
           (t
        (case digit
          (no (format nil "~r-~:r~P" num den num))
          (t (format nil "~s/~s" num den))))))
    ((and (stringp lex)
          (not (string= lex "nil"))
          (not (string= lex "none"))) lex)
    (t (format nil "~s/~s" num den))))


;; CBC - Version 2.3: Create a numeric range
(defun MORPH-RANGE (lex low high dash digit)
  "Return a string for a range"
  (cond ((and (integerp low) (integerp high))
           (case digit
              (no    (format nil "~r~a~r" low dash high))
              (roman (format nil "~@R~a~@R" low dash high))
              (t     (format nil "~s~a~s" low dash high))))
    ((and (stringp lex)
          (not (string= lex "nil"))
          (not (string= lex "none"))) lex)
    (t (format nil "~s~a~s" low dash high))))


;; ------------------------------------------------------------
;; List-punctuation
;; ------------------------------------------------------------

(defun list-punctuation (punctuation path lstring)
  "Punctuation is an fd with 5 relevant features: before, after,
       capitalize, append-before and append-after.
       The value of before, after, append-before and append-after must be a
       string of punctuation. Capitalize can be yes or no.  Default is no.
       The string in append-before is appended to the first word in lstring.
       The string in append-after is appended to the last word in lstring.
       The strings in before and after are added as new punctuation words
       before and after lstring.
       Path is the path from the root of the total fd to punctuation.
       Lstring is a list of string.
       Returns a list of strings with punctuation before and
       after lstring as indicated by punctuation."
  (setq lstring (trim-lstring lstring))
  ;; CBC - Version 2.3: Handle possessives here
  (if (and (equal punctuation '((AFTER "*apostrophe-s*" :E)))
           (< 5 (length (first (last lstring))))
           (equal (subseq (first (last lstring)) (- (length (first (last lstring))) 5) (length (first (last lstring))))
                  "s****"))
      (setq punctuation '((AFTER "*apostrophe*" :E))))
  (cond ((null punctuation) lstring)
    (t
     ;; CBC - Version 2.3: An attempt to do multiple punctuation on left or right
     (let ((before (incr-gdp 'before punctuation path))
           (after  (incr-gdp 'after  punctuation path))
           (append-before (incr-gdp 'append-before punctuation path))
           (append-after  (incr-gdp 'append-after  punctuation path))
           (capitalize (incr-gdp 'capitalize punctuation path)))
       (unless (or (null lstring)
                   (null capitalize)
                   (eq 'none capitalize)
                   (eq 'no capitalize))
         (setf (car lstring) (string-capitalize (car lstring) :end 1)))
           (when (null lstring)
             (setf lstring (list "")))
           (when (stringp append-before)
             (setf (car lstring)
                   (concatenate 'string append-before (car lstring))))
           (when (stringp append-after)
             (setf (car (last lstring))
                   (concatenate 'string (car (last lstring)) append-after)))
       (when (stringp before)
         (setq lstring (cons before lstring)))
       (when (stringp after)
         (setq lstring (append lstring (list after))))
       lstring))))

(defun trim-lstring (lstring)
  (remove "" lstring :test #'string-equal))

;; ------------------------------------------------------------
;; Pluralize (written by Jay Meyer)
;; ------------------------------------------------------------

;; CBC - Version 2.3: Now need to handle pluralization with acronyms and capitalizations
(defun PLURALIZE (word-string acronym-np)
  "Handles word-string ending in ch, sh, o, s, x, y, and z
  as well as regular forms."
  (if (< 2 (length word-string))
    (let* ((n (length word-string))
           (next-to-final (elt (string-downcase word-string) (- n 2)))
           (final (elt (string-downcase word-string) (1- n)))
           (up  (equal word-string (string-upcase word-string)))
           (down  (equal word-string (string-downcase word-string)))
           (acronym  (or (equal acronym-np 'yes) (search "." word-string) (search "&" word-string)))
           (s-ending (cond (acronym "s")
                           ((and up (not down)) "S")
                           (t  "s")))
           (es-ending (if up "ES" "es"))
           (ies-ending (if up "IES" "ies")))
      (case final
            ((#\s #\z #\x) (concatenate 'string word-string es-ending))
            (#\h (if (member next-to-final '(#\c #\s))
                     (concatenate 'string word-string es-ending)
                     (concatenate 'string word-string s-ending)))
            (#\o (cond ((member2 next-to-final '(#\n #\c #\d))  (concatenate 'string word-string s-ending))
                       ((not (vowel next-to-final))  (concatenate 'string word-string es-ending))
                       (t  (concatenate 'string word-string s-ending))))
            (#\y (if (not (vowel next-to-final))
                     (concatenate 'string (subseq word-string 0 (1- n)) ies-ending)
                     (concatenate 'string word-string s-ending)))
            (t (concatenate 'string word-string s-ending))))
    (string-append word-string
                   (cond ((equal word-string "")  "")
                         ((equal word-string "%")  "")
                         ((equal (string-upcase word-string) word-string)  "S")
                         (t  "s")))))




;; -----------------------------------------------------------------------
(provide "linearize")
;; -----------------------------------------------------------------------
