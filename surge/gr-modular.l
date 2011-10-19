;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:  -*-
;;; ------------------------------------------------------------
;;; File        : GR-MODULAR.L
;;; Description : Update of gr.l:
;;;               For advisor system using def-alt.
;;; Author      : Michael Elhadad
;;; Created     : 18 Dec 91
;;; Modified    : 16 Jul 92 - added NP subtleties (partitive, measure...)
;;;               19 Aug 92 - added extended NP categories (JR)
;;;               ?? Nov 92 - added date and addresses (JR)
;;;               18 Jan 93 - removed semrs & kinds (JR)
;;;               21 Dec 93 - added np-propagate in NP (merged in 2.2)
;;;                5 Jul 95 - SURGE 2.2 VERSION
;;;                           added adv-p
;;;               05 May 96 - added (case objective) to np in (cat pp) (Yael)
;;;               24 Sep 96 - added call to date and address in np.
;;;               31 Mar 97 - added support for comparative/superlative (Yael)
;;;               25 May 05 SURGE 2.3 VERSION
;;;                         - added written dialogue utterances
;;;                         - added absolute-time, numeric-range categories
;;;                         - added new AP qualifiers and modifiers
;;;                         - added new PP complements and modifiers
;;;                         - added new ADVP complements and modifiers
;;; ------------------------------------------------------------

;(in-package :FUG5)

;; ============================================================
;; GRAMMAR
;; ============================================================

(def-grammar gr ()
  ;; These are completely determined by the morphology features
  ;; they are not unified with the grammar
  (register-categories-not-unified
   'cat
   '(verb punctuation prep conj relpro modal))

  (clear-bk-class)
  ;; (define-bk-class 'dative-move 'dative-move)

  (setf *any-at-unification* nil)

  (setq *realize-grammar*
   '((alt (:wait {^ cat})
     ;;==============================================================
     ;; 01 CAT DS : discourse segment -------------------------------
     ;;==============================================================
     ;; A structured paragraph with directive/subordinate and optional
     ;; connective between them (can be recursive).
   (((cat #(under ds))
     (alt simple (:index simple)
       ;; simple DS: only propositional content in the whole segment.
       (((simple yes)
         ;(directive none)
         ;(subordinate none)
         ;(connective none)
         (cset ((= pc)))
         (pattern (pc)))
    ((simple no)
     (cset ((= connective directive subordinate)))
     (alt connective
         (((connective none)
           (subordinate none)
           (pattern (directive)))
          ((connective ((cat connective) (break-sentence no)))
           (subordinate given)
           (subordinate ((punctuation ((after ",")))))
           (alt ds-order (:wait {^ connective order})
                         (:index (connective order))
             (((connective ((order dcs)))
               (pattern (directive connective subordinate)))
              ((connective ((order scd)))
               (pattern (subordinate connective directive))))))
              ((connective ((cat connective) (break-sentence yes)))
               (subordinate given)
               (pattern (subordinate connective directive))
               (connective ((punctuation ((capitalize yes)))))
               (subordinate ((punctuation ((after "."))))))
              ((connective sentence)
               (subordinate given)
               (pattern (subordinate directive))
               (directive ((punctuation ((capitalize yes)))))
               (subordinate ((punctuation ((after "."))))))))))))


    ;;==============================================================
    ;; 02 CAT UTTERANCE : written dialogue -------------------------
    ;;==============================================================
    ((cat utterance)
     (alt simple (:index simple)
       (((simple yes)
         (cset ((= pc)))
         (pattern (pc)))
        ((simple no)
         (opt (({open-double-quote} "\"")
               ({close-double-quote} "\"")))
         (cset ((= utt1 sayer utt2 front-adverbial-1 end-adverbial-1)))
         (alt ordering
             (((sayer none) (utt2 none) (utt1 given) (pattern (front-adverbial-1 utt1 end-adverbial-1))
               (alt (((front-adverbial-1 given)
                      (front-adverbial-1 ((cat #(under np)))))
                     ((front-adverbial-1 given)
                      (front-adverbial-1 ((cat clause))))
                     ((front-adverbial-1 none))))
               (alt (((end-adverbial-1 given)
                      (end-adverbial-1 ((cat #(under np)))))
                     ((end-adverbial-1 given)
                      (end-adverbial-1 ((cat clause))))
                     ((end-adverbial-1 none))))
               (utt1 ((formality {^ ^ formality})
                      (punctuation ((capitalize yes)))
                      (alt (((mood given) (mood #(under interrogative))
                             (question-mode given) (question-mode direct)
                             (punctuation ((before {open-double-quote})
                                           (after "?''"))))
                            ((punctuation ((before {open-double-quote})
                                           (after ".''"))) (emphatic no))
                            ((punctuation ((before {open-double-quote})
                                           (after "!''")))))))))
              ((sayer none) (utt1 none) (utt2 given) (pattern (utt2)))
              ((sayer given) (utt1 given) (utt2 none)
               (sayer ((order second)
                       (alt (((make-verb-fronted no) (verb-fronted no)) ((verb-fronted yes))))))
               (pattern (front-adverbial-1 utt1 sayer end-adverbial-1))
               (alt (((front-adverbial-1 given)
                      (front-adverbial-1 ((cat #(under np)))))
                     ((front-adverbial-1 given)
                      (front-adverbial-1 ((cat clause))))
                     ((front-adverbial-1 none))))
               (alt (((end-adverbial-1 given)
                      (end-adverbial-1 ((cat #(under np)))))
                     ((end-adverbial-1 given)
                      (end-adverbial-1 ((cat clause))))
                     ((end-adverbial-1 none))))
               (utt1 ((formality {^ ^ formality})
                      (punctuation ((capitalize yes)))
                      (alt (((mood given) (mood #(under interrogative))
                             (question-mode direct)
                             (punctuation ((before {open-double-quote})
                                           (alt ((({open-double-quote} "``") (after "?''"))
                                                 (({open-double-quote} "\"") (after "?\""))
                                                 ((after "?")))))))
                            ((emphatic no)
                             (punctuation ((before {open-double-quote})
                                           (alt ((({open-double-quote} "``") (after ",''"))
                                                 (({open-double-quote} "\"") (after ",\""))
                                                 ((after ",")))))))
                            ((punctuation ((before {open-double-quote}) (after "!''")))))))))
              ((sayer given) (utt1 given) (utt2 none)
               (sayer ((alt (((make-verb-fronted no) (verb-fronted no)) ((verb-fronted yes))))))
               (sayer ((punctuation ((capitalize yes) (after ((alt ("," ":" none))))))))
               (pattern (front-adverbial-1 sayer utt1 end-adverbial-1))
               (alt (((front-adverbial-1 given)
                      (front-adverbial-1 ((cat #(under np)))))
                     ((front-adverbial-1 given)
                      (front-adverbial-1 ((cat clause))))
                     ((front-adverbial-1 none))))
               (alt (((end-adverbial-1 given)
                      (end-adverbial-1 ((cat #(under np)))))
                     ((end-adverbial-1 given)
                      (end-adverbial-1 ((cat clause))))
                     ((end-adverbial-1 none))))
               (utt1 ((formality {^ ^ formality})
                      (opt ((punctuation ((capitalize yes)))))
                      (alt (((mood given) (mood #(under interrogative))
                             (question-mode direct)
                             (punctuation ((before {open-double-quote})
                                           (alt ((({open-double-quote} "``") (after "?''"))
                                                 (({open-double-quote} "\"") (after "?\""))
                                                 ((after "?")))))))
                            ((emphatic no)
                             (punctuation ((before {open-double-quote})
                                           (alt ((({open-double-quote} "``") (after ".''"))
                                                 (({open-double-quote} "\"") (after ".\""))
                                                 ((after ".")))))))
                            ((punctuation ((before {open-double-quote}) (after "!''")))))))))
              ((sayer given) (utt1 given) (utt2 given)
               (sayer ((alt (((make-verb-fronted no) (verb-fronted no)) ((verb-fronted yes))))))
               (pattern (utt1 sayer utt2))
               (utt1 ((formality {^ ^ formality})
                      (alt (((mood given) (mood #(under interrogative))
                             (question-mode direct)
                             (question-split given) (question-split yes)
                             (punctuation ((before {open-double-quote}) (capitalize yes)
                                           (alt ((({open-double-quote} "``") (after "?''"))
                                                 (({open-double-quote} "\"") (after "?\""))
                                                 ((after "?")))))))
                            ((emphatic no)
                             (punctuation ((before {open-double-quote}) (capitalize yes)
                                           (alt ((({open-double-quote} "``") (after ",''"))
                                                 (({open-double-quote} "\"") (after ",\""))
                                                 ((after ",")))))))
                            ((punctuation ((before {open-double-quote}) (capitalize yes)
                                           (after "!''"))))))))
               (sayer ((punctuation ((after ",")))))
               (utt2 ((formality {^ ^ formality})
                      (alt (((mood given) (mood #(under interrogative))
                             (question-mode direct)
                             (punctuation ((before {open-double-quote}) (capitalize yes)
                                           (after "?''"))))
                            ((question-mode direct) (mood #(under interrogative))
                             (question-mode {^ ^ utt1 question-mode})
                             (punctuation ((before {open-double-quote}) (capitalize no)
                                           (after "?"))))
                            ((cat ap)
                             (question-mode direct)
                             (mood {^ ^ utt1 mood})
                             (mood #(under interrogative))
                             (question-mode {^ ^ utt1 question-mode})
                             (punctuation ((before {open-double-quote}) (capitalize no)
                                           (after "?''"))))
                            ((punctuation ((before {open-double-quote}) (capitalize no) ({open-double-quote} "")
                                           (after "."))))
                            ((punctuation ((before {open-double-quote}) (capitalize no)
                                           (alt ((({open-double-quote} "``") (after ".''"))
                                                 (({open-double-quote} "\"") (after ".\""))
                                                 ((after ".")))))))
                            ((punctuation ((before {open-double-quote}) (capitalize no)
                                           (after "!''"))) (emphatic no)))))))))))))

    ;;==============================================================
    ;; 03 CAT CONNECTIVE : discourse element -----------------------
    ;;==============================================================
    ((cat connective)
     (pattern (head))
     (head ((lex {^2 lex}) (cat conj)))
     (alt connective-lex (:index (head lex))
       (((head ((lex "so"))) (order scd))
        ((head ((lex "and"))) (order scd))
        ((head ((lex "since"))) (order dcs))
        ((head ((lex "because"))) (order dcs))
        ((head ((lex "although"))) (order dcs))
        ((head ((lex "but"))) (order scd))
        ((head ((lex "therefore"))) (order scd)))))

    ;; first only simple constituents - complex follows
    ;;==============================================================
    ;; 04 CAT CLAUSE : clause --------------------------------------
    ;;==============================================================
    ((:& simple-clause)
     (cset ((- semr kind))))

    ;;==============================================================
    ;; 05 CAT VERB-GROUP -------------------------------------------
    ;;==============================================================
    ((:& simple-verb-group)
     (cset ((- semr kind))))

    ;;==============================================================
    ;; 06 CAT NP ---------------------------------------------------
    ;;==============================================================
    ;; If there is a gap in the constituent, don't do anything at all.
    ((cat #(under np))
     (alt np-gap (:ignore-when gap)
       (((gap none)
         (alt type-of-np (:index cat)
            (((:& partitive))
             ((:& trivial-proper)) ;; For proper nouns (no fuss, efficient)
             ((:& measure))
             ((:& date))
             ((:& address))
             ;; CBC - Version 2.3: Added numeric-range, absolute-time, institution
             ((:& numeric-range))
             ((:& absolute-time))
             ((:& institution-name))
             ((:& simple-np)))))
        ((gap #(under yes))
         (alt type-of-np-gap (:index cat)
            (((cat #(under partitive)))
             ((cat #(under trivial-proper)))
             ((cat #(under measure)))
             ((cat #(under date)))
             ((cat #(under address)))
             ;; CBC - Version 2.3: Added numeric-range, absolute-time
             ((cat #(under numeric-range)))
             ((cat #(under absolute-time)))
             ;; Here do everything done in conj simple-np except the alts
             ;; affecting subconstituents of the NP which are useless.
             ;; Still need to put the NP in canonical form.
             ((cat #(under np))
              (:& np-propagate)
              (:! np-type)
              (:& np-number)))))))
     (cset ((- semr kind))))

    ;;==============================================================
    ;; 07 CAT NP-HEAD ----------------------------------------------
    ;;==============================================================
    ((cat np-head)
     (cset ((- semr kind)))
     (complex none)
     (alt (((cat noun) (generic-cat noun))
       ((:& noun-compound))
       ((:& measure))
       ((:& partitive))
       ((:& person-name))
       ((:& team-name)))))

    ;; ==============================================================
    ;; 08 CAT AP : for adjectival phrases ---------------------------
    ;; ==============================================================
    ((cat simple-ap)
     (cset ((- semr kind)))
     (complex none)
     (generic-cat ap)
     (head ((cat adj)
            (concept {^ ^ concept})
            (polarity {^ ^ polarity})
            (comparative {^ ^ comparative}) ;; YD add these
            (superlative {^ ^ superlative})
            (inflected {^ ^ inflected})
            (orientation {^ ^ orientation})
            ;(degree {^ ^ degree})
            (lex {^ ^ lex})))
     ;; YD superlative and compartive must be mutual exclusive
     (alt (((comparative no)
            (superlative no))
           ((comparative yes)
            (superlative no)
            (alt (((inflected no)
                   (intensifier ((cat phrase) (lex "more"))))
                  ((inflected yes)))))
           ((superlative yes)
            (comparative no)
            (alt (((inflected no)
                   (intensifier ((cat phrase) (lex "most"))))
                  ((inflected yes)))))))
     ;; "light blue" (light is the classifier)
     (alt (((classifier none))
           ((classifier given)
            (superlative no)    ;; YD assume a superlative cannot have a classifier.
            (classifier ((cat ((alt (adj #(under np) #(under np-head)))))
                         (synt-funct classifier))))
           ((classifier ((cat list))))))
     ;; "visible in the cutaway view" (pp qualifier)
     ;; CBC - Version 2.3: Added NP "visible each day"
     ;;                    Added infinitive "ready to go"
     ;;                    Added ADV(P) from Penn TreeBank
     (alt (((qualifier none))
           ((qualifier given)
            (qualifier ((cat pp))))
           ((qualifier given)
            (qualifier ((cat #(under np)))))
           ((qualifier given)
            (qualifer ((cat clause) (mood infinitive))))
           ((qualifier given)
            (qualifier ((cat adv-p))))
           ((qualifer given)
            (qualifier ((cat adv))))))
     ;; modifier is an adverb: can be intensifier or detensifier
     ;; CBC - Version 2.3: Added other modifier categories from Penn TreeBank
     (alt (((modifier none))
           ((modifier given)
            (modifier ((cat adv))))
           ((modifier given)
            (modifier ((cat adv-p))))
           ((modifier given)
            (modifier ((cat pp) (punctuation ((before ",") (after ","))))))
           ((modifier given)
            (modifier ((cat #(under np)))))
           ((modifier ((cat adv))))))
     ;; CBC - Version 2.3: Added a post-modifier as found in the Penn TreeBank
     (alt (((post-modifier none))
           ((post-modifier given)
            (post-modifier ((cat adv))))))
     %ap%
     (alt (((qualifier given) (that-binder none))
           ((qualifier given) (qualifier ((restrictive given) (restrictive yes)))
            (that-binder ((cat adv) (lex ((alt ("that" given)))))))
           ((that-binder ((gap yes))))))
     (pattern (modifier classifier head post-modifier that-binder qualifier)))

    ;; ==============================================================
    ;; 09 CAT PP : for prepositional phrases ------------------------
    ;; ==============================================================
    ((cat simple-pp)
     (cset ((- semr kind)))
     (complex none)
     (generic-cat pp)
     ;; CBC - Version 2.3: Added other possibilities besides Prep + NP
     ;;                    "just until noon each day"
     (pattern (adv prep np compl))
     (prep ((cat prep) (lex given)))
     (alt (((compl none))
           ((compl ((cat adv) (lex given))))
           ((compl ((cat adv-p) (head given))))
           ((compl ((cat #(under np)) (lex given))))
           ((compl ((cat list) (distinct given))))
           ((compl ((cat clause) (proc given))))
           ((compl ((cat date))))))
     (alt (((adv none))
           ((adv ((cat adv) (lex given))))
           ((adv ((cat pp) (np ((lex given))))))
           ((adv ((cat np) (lex given))))))
     (np ((alt (((cat np) (case objective))
                ((cat np) (case reflexive))
                ((cat list))
                ((cat date))
                ((cat adv) (lex given))  ;; "Until recently"
                ((cat adv-p))            ;; "Until a few years ago"
                ;((cat clause) (binder given) (binder ((cat adv) (lex "what"))))
                ((cat clause) (mood finite))
                ((cat clause) (mood imperative))
                ((cat clause) (mood wh))
                ((cat clause) (binder given) (mood bound))
                ((cat clause) (mood present-participle)))))))

    ;;==============================================================
    ;; 10 CAT DET : for articles -----------------------------------
    ;;==============================================================

    ((:& det)
     (cset ((- semr kind))))

    ((:& fraction)
     (cset ((- semr kind))))

    ;; ==============================================================
    ;; 11 CAT LIST : for agglutinated constituents ------------------
    ;; ==============================================================
    ;; List is for an agglutination of heterogeneous constituents all
    ;; playing together the same syntactic function in a larger
    ;; constituent.
    ;; For example, a list of describers or qualifiers in an NP.
    ;; Each element of the list can be of a different cat (unlike
    ;; conjunction).
    ;; Lists have one main feature:
    ;; elements: a list of features in car/cdr form (~ macro is useful).
    ;; Just recurse on all elements of the list with no additional processing
    ((cat list)
     (elements {^ distinct})  ;; for compatibility with complex
     (cset ((- semr kind)))
     (alt list
       (:demo"How many elements are there in the list?")
       (((elements none))                           ;; 0 elements
    ((elements ((car given) (cdr none)))        ;; 1 elements
     (first {^  elements car})
     (cset (first))                 ;; to eliminate any others
     (pattern (first)))
    ((first {^  elements car})      ;; more
     (first given)
     (rest ((cat list)
        (elements {^ ^ elements cdr})
        (elements given)))
     (cset (first rest))
     (pattern (first rest))))))


    ;; ==============================================================
    ;; 12 CAT COMPLEX : for complex constituents --------------------
    ;; ==============================================================
    ((:& complex)
     (cset ((- semr kind))))

    ((cat simple-date)
     (complex none)
     (:& date))

    ;; ==============================================================
    ;; 13 CAT SET : for semantic description of sets ----------------
    ;; ==============================================================
    ;; ((:& set))
    ((cat set))

    ((cat adj)
     (cset ((- semr kind))))

    ;; ==============================================================
    ;; 14 CAT ADV : for adverbial phrases --------------------------
    ;; ==============================================================
    ((cat adv)
     (generic-cat adv)
     (cset ((- semr kind)))
     ;(complex none)
     (alt adv-type (:index cat)
       (((cat #(under detensifier))
         (lex ((ralt ("quite" "pretty" "rather" "somehow")))))
       ((cat #(under intensifier))
        (lex ((ralt ("very" "extremely")))))
       ((cat #(under adv-p))
        ;; CBC - Version 2.3: Added other possibilities besides head + complement
        ;;                    "not really completely in the dark"
        (alt (((alt (((compl none))
                     ((compl given)
                      (compl ((cat adv) (lex given))))
                     ((compl given)
                      (compl ((cat adj) (lex given))))
                     ((compl given)
                      (compl ((cat pp))))
                     ((compl given)
                      (compl ((cat #(under np)))))
                     ((compl given)
                      (compl ((cat #(under np))
                              (alt (((cardinal given))
                                    ((qualifier given))
                                    ((determiner given)))))))
                     ((compl given)
                      (compl ((cat clause))))
                     ((compl given)
                      (compl ((cat list))))))
               (alt (((modifier none))
                     ((modifier given)
                      (modifier ((cat adv) (lex given))))
                     ((modifier given)
                      (modifier ((cat #(under np)))))))
               (alt (((polarity none))
                     ((polarity given)
                      (not ((cat adv) (lex "not"))))))
               (pattern (not modifier head compl))
               (cset (compl head modifier))
               (head ((alt (((cat adv) (lex given))
                            ((cat adv-p) (head given))
                            ((cat #(under np))))))))
              ((np given)
               (pattern (head np))
               (cset (np))
               (head ((alt (((cat adv))
                            ((cat adv-p))))))
               ))))
       ((cat adv)
        (modifier given)
        (modifier ((alt (((cat adv) (lex given))
                         ((cat #(under np)) (lex given))))))
        (cset ((- {modifier cat})))
        (head ((cat {^ ^ cat})
               (lex {^ ^ lex})))
        (alt (((polarity none))
              ((polarity given)
               (not ((cat adv) (lex "not"))))))
        (pattern (not modifier head)))
       ((cat adv)))))

    ;; ==============================================================
    ;; 15 OTHER : Misc categories ignored by the grammar and
    ;;            recognized by the morphology component.
    ;; ==============================================================
    ((cat phrase) (cset ((- semr kind))))
    ((cat article))
    ((cat pronoun))
    ;; CBC - Version 2.3: Added the interjection category
    ((cat interjection))

    ((cat cardinal)
     ;; Stylistic rule: integers less than 10 in letters, others in digits.
     (alt cardinal-value
     (((value given)
       (control (and (integerp #@{^ value})
             (> #@{^ value} 0)
             (< #@{^ value} 11)))
       (digit no))
      ((digit yes))
      ((digit #(under roman)))
      ;; CBC - Version 2.3: Added a second "digit no" for when control is not passed
      ((digit no)))))

    ((cat ordinal)
     (alt (((value given)
        (control (and (integerp #@{^ value})
              (> #@{^ value} 0)
              (< #@{^ value} 11)))
        (digit no))
       ((value +)
        (lex ((ralt ("next" "following" "subsequent")))))
       ((value -)
        (lex ((ralt ("preceding")))))
       ((value <>)
        (lex "other"))
       ((value last)
        (lex "last"))
       ((digit yes))
       ((digit #(under roman))))))

    ;; ==============================================================
    ;; 16 DOMAIN : Domain dependent categories.
    ;; ==============================================================
    ((cat score)
     (hi ((cat cardinal)
      (digit yes)
      (value {^ ^ win})))
     (lo ((cat cardinal)
      (digit yes)
      (value {^ ^ lose})))
     (to ((cat phrase)
      (lex "-")))
     (pattern (hi to lo)))

    )))))


(gr)


;; ============================================================
;;(provide "gr-modular")
;; ============================================================
