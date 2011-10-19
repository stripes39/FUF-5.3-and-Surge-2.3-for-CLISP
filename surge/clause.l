;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         clause.l
;;; Description:  Grammatical systems for clause
;;; Author:       Michael Elhadad & Jacques Robin
;;; Created:      19 Dec 1991
;;; Modified:     07 Jul 92 - added past-participle mood treatment
;;;               14 Jul 92 - added Karen's constraint on pronoun/dative-move
;;;               18 Aug 92 - added adverb
;;;               17 Nov 92 - Jacques Robin commented out waits for lex-cat
;;;               27 Nov 92 - Jacques Robin added predicate-modifiers,
;;;                           and disjuncts, subdivided adjuncts into
;;;                           predicate-adjuncts and sentence-adjuncts,
;;;                           moved out the mood code to a mood.l file
;;;                           Also changed patterns.
;;;                5 Jul 95 - SURGE 2.2 VERSION
;;;                         - added clause-level/embedded which is different from
;;;                           scoped (eg, "Who is coming" is scoped but not embedded);
;;;                           embedded is necessary to determine if subject inversion
;;;                           (eg,  "What happens" vs. "What do you think happens")
;;;                           is required
;;;                         - added alt focus
;;;                6 Nov 95 - removed hard-wired "by" for passive-prep in
;;;                           alt by-obj-cat
;;;               12 May 96 - changed object-subcat to allow wh-clauses
;;;                           as in "Ask him what it is".
;;;               25 May 05 - SURGE 2.3 VERSION
;;;                           Added patterns for addressees and interjections
;;;                           Experimental clause-internal adverb positions
;;;                           Added formality features for verb contractions
;;;                           Added direct vs. indirect question orderings
;;; -----------------------------------------------------------------------

;(require "transitivity" "transitivity.l")
;(require "voice" "voice.l")
;(require "adverbial" "adverbial.l")
;(require "mood" "mood.l")

;(in-package :FUG5)

;;; BRANCH FOR SIMPLE-CLAUSE
(def-conj simple-clause
  (cat simple-clause)
  (generic-cat clause)
  (complex none)

  ;; Some shorthands that are used for convenience...
  (verb {^ process})
  (proc {^ process})
  (partic {^ participants})
  (circum   {^ circumstances})
  (pred-modif {^ predicate-modifiers})
  (process ((type {^ process-type})))
  (process-type {^ process process-type})
  (lex-roles {^ lexical-roles})


  ;; General structure of a clause
  ;; lexical-roles      : semantic roles specific to a lexical item
  ;; participants       : semantic arguments of the process type
  ;; oblique            : obliqueness hierarchy of semantic arguments
  ;; synt-roles:        : syntactic arguments
  ;; predicate-modifiers: optional semantic roles refining the specification
  ;;                      of the clause predicate
  ;; circumstances      : the optional semantic roles specifying the setting of the
  ;;                      the clause as a whole
  ;; disjuncts          : peripheral movable optional syntactic constituents
  ;; sentence-adjuncts  : central movable optional syntactic constituents
  ;; predicate-adjuncts : non-movable optional syntactic constituents
  ;;
  ;; Processing of semantic arguments:
  ;; Map from partic -> oblique -> synt-roles.
  ;; OR Map from lexical-roles -> oblique from lexicon.
  ;; First stage is transitivity, second is voice.
  ;; Government pattern of verbs is specified under subcat which is
  ;; unified with synt-roles (ala Mel'cuk and vaguely subcat of HPSG).
  (participants ((fset (agent affected created range
                    processor phenomenon
            sayer addressee verbalization
            carrier attribute
            identified identifier
            located location time
            possessor possessed))))
  ;; Choice is either lexical-roles or partic or none but not both
  (alt type-of-roles
      (((lexical-roles none)
        (partic given))
       ((lexical-roles given)
        (partic none)
        (process-type lexical))
       ((lexical-roles none)
        (partic none)
        (alt (((process-type natural-phenom))
              ;; JR-added 1/19/93 to avoid stub roles in mono-role imperative clauses
              ((mood #(under imperative))
               (process ((alt (((type #(under material))
                                (effective #(under no)))
                               ((type #(under mental))
                                (transitive #(under no))))))))
              ((partic any)))))))
  (oblique ((fset (1 2 3 4 5 6 7 8))))
  (synt-roles ((fset (subject object iobject subj-comp obj-comp dative by-obj))))
  (innermost-role {^ oblique 1})
  (:! mood)
  (:! transitivity)
  (:! voice)
  ;; END OF TREATMENT OF INHERENT PARTICIPANTS

  ;; Process optional participants
  (:! old-circum)
  (:! predicate-modifiers)
  (:! circumstantials)
  (:& relaters)
  (:! interjections)
  (:! addressee)
  (:! response)

  ;;&&& CBC - Version 2.3: Appropriately place addressees, interjections, responses
  (pattern (dots stop-header dots front-adverbial-1 front-adverbial-2
            front-adverbial-3 front-adverbial-4 front-adverbial-5
            dots start dots stop-kernel end-adverbial-1 end-adverbial-2
            end-adverbial-3 end-adverbial-4 end-adverbial-5 dots))
  (alt (((addressee none) (mood given) (mood bound)
         (pattern (dots stop-header dots interjection front-adverbial-1 dots response dots binder dots start dots)))
        ((addressee none)
         (pattern (dots stop-header dots interjection dots response dots binder dots front-adverbial-1 dots)))
        ((addressee ((position front)))
         (pattern (dots stop-header dots interjection dots response dots addressee dots
                   binder dots front-adverbial-1 dots)))
        ((addressee ((position end)))
         (pattern (dots stop-header dots interjection dots response dots binder dots
                   front-adverbial-1 dots end-adverbial-5 dots addressee dots)))))
  (cset ((- stop-header start stop-kernel)))

  ;;*** CBC - Version 2.3: Experiment with multiple adverb positions.
  ;;                    Motivated by Penn TreeBank placements
  (alt pre-adverb (:index pre-adverb)
    (((pre-adverb none))
     ((pre-adverb given)
      (pre-adverb ((cat adv))))
     ((pre-adverb given)
      (pre-adverb ((cat pp))))
     ((pre-adverb given)
      (pre-adverb ((cat clause))))
     ((pre-adverb given)
      (pre-adverb ((cat list))))
     ((pre-adverb given)
      (pre-adverb ((cat clause))))
     ((pre-adverb given)
      (pre-adverb ((cat #(under np)))))))

  (alt internal-adverb (:index internal-adverb)
    (((internal-adverb none))
     ((internal-adverb given)
      (internal-adverb ((cat adv)))
      (internal-adverb {^ process internal-adverb}))))

  (alt adverb (:index adverb)
    (((adverb none))
     ((adverb given)
      (adverb ((cat adv))))
     ((adverb given)
      (adverb ((cat pp))))
     ((adverb given)
      (adverb ((cat list))))
     ((adverb given)
      (adverb ((cat #(under np)))))))

  (alt post-adverb (:index post-adverb)
    (((post-adverb none))
     ((post-adverb given)
      (post-adverb ((cat adv))))
     ((post-adverb given)
      (post-adverb ((cat pp))))
     ((post-adverb given)
      (post-adverb ((cat ap))))
     ((post-adverb given)
      (post-adverb ((cat list))))
     ((post-adverb given)
      (post-adverb ((cat clause))))
     ((post-adverb given)
      (post-adverb ((cat #(under np)))))))

  ;; DISPLACED CONSTITUENT PROCESSING
  ;; for interrogative and relative mood, the constituent under scope
  ;; is displaced.  We handle it in this alternation.
  (alt scoped-determination (:index scope)
    (((scope none))
     ((scope given) (scope ((clause-level ((scoped yes))))))))

  ;; Every filler of a participant or an adjunct or a qualifier  will get
  ;; a synt-funct set to something else (subject, object, subj-comp, obj-comp,
  ;; qualifier) so it will not unify with main-clause here.
  (alt embedded-determination (:wait synt-funct)
    (((synt-funct main-clause) (embedded no))
     ((control (not (equal #@{^ synt-funct} 'main-clause)))
      (embedded yes))))

  (:! agentless)
  (:! displaced-constituent)

  ;; If dative-move is not bound yet, it should be yes
  ;; Do not use dative-move if object is a pronoun
  ;; * Mary gave the library it.
  ;; Mary gave it to the library.
  (process ((dative-move {^2 dative-move})))
  (alt dative-move-default (:bk-class dative-move)
    (((dative-move yes)
      (oblique ((3 given)
                (3 ((cat ((alt (common proper)))))))))
     ((process-type lexical)
      (dative-move yes))
     ((dative-move no))))

  ;; check cat of each syntactic role subcategorization system
  (:& subcat)

  ;; START OF GENERAL THINGS: ORDERING PLUS AGREEMENT
  ;; General things: arrange syntactic roles together and do the agreements.
  ;; The patterns are here of course.

  ;; Focus is a pointer to a semantic constituent on which focus is placed.
  ;; In general, we want to make the syntactic constituent which realizes the
  ;; focus the subject of the clause.  The focus-given alt checks which is
  ;; the syntactic constituent realizing focus.  realization can be:
  ;; HEAD: this means that the pointer is pointing to a semantic constituent which
  ;; is a trace in a relative clause, and is therefore realized by the head of the NP.
  ;; In this case, we want the trace to be the subject.
  ;; A PATH: to the syntactic constituent realizing the focus.
  ;; Note: this only works for participants who can become subjects.
  ;; other fronting devices (it-clefts dislocation etc) are not implemented.
  (alt focus
      (((focus none))
       ((focus given)
    (alt focus-given
        (((focus ((synt-funct given)))
          (focus {^ synt-roles subject}))
         ((focus ((realization head)))
          (focus {^ synt-roles subject}))
         ((focus ((synt-funct none)
                  (realization {^2 synt-roles subject})))))))))

  ;; Number and person agreement (verb is process)
  (verb ((cat verb-group)
         (modality {^ ^ modality})
         (epistemic-modality {^ ^ epistemic-modality})
         (deontic-modality {^ ^ deontic-modality})
         (tense  {^ ^ tense})
         (polarity {^ ^ polarity})
         ;;*** CBC - Version 2.3: New propagations for adverbs and formality
         (pre-adverb {^ ^ pre-adverb})
         (adverb {^ ^ adverb})
         (post-adverb {^ ^ post-adverb})
         (formality {^ ^ formality})
         (insistence {^ ^ insistence})
         (person {^ ^ synt-roles subject person})
         (number {^ ^ synt-roles subject number})))

  ;; particle is for verbs like "take off"
  ;; This is for MOVABLE particles only.
  ;; non-movable particles should be treated as in the example in ir8.l
  (cset ((- particle)))
  (alt particle (:demo "Does the verb have a particle?")
                (:wait {^ verb particle})
    (((verb ((particle none))))
     ((verb ((particle given)))
      (particle ((cat adv)
                 (lex {^ ^ verb particle})))
      ;;*** CBC - Version 2.3: More formalized particle control
      (alt particle-moves-or-stays
         (((verb ((particle-position front)))
           (pattern (dots process dots particle dots {^ synt-roles object}
                     dots {^ synt-roles subj-comp} dots)))
          ((verb ((particle-position end)))
           (pattern (dots {^ synt-roles object} dots
                     {^ synt-roles subj-comp} dots {^ synt-roles subj-comp} particle dots end-adverbial-1 dots))))))))

  ;; Default constituent ordering
  ;(pattern
  ; (dots start {^ synt-roles subject} dots
  ;       process dots
  ;       {^ synt-roles iobject} dots
  ;       {^ synt-roles object} dots
  ;       particle dots
  ;       {^ synt-roles subj-comp} {^ synt-roles obj-comp} dots
  ;       {^ synt-roles by-obj} {^ synt-roles dative} stop-kernel dots))

  ;;*** CBC - Version 2.3: Add clausal orderings for indirect vs. direct questions
  (alt (((question-type direct))
        ((question-type indirect))))
  (alt question-type-pattern
     (((mood wh)
       (question-type indirect)
       (pattern
          (dots start dots pre-adverb dots
              process dots post-adverb dots
              {^ synt-roles subject} dots
              {^ synt-roles iobject} dots
              {^ synt-roles object} dots
              ;particle dots
              {^ synt-roles subj-comp} {^ synt-roles obj-comp} dots
              {^ synt-roles by-obj} {^ synt-roles dative}
              stop-kernel dots
              )))
      ((verb-fronted no)
           (pattern
              (dots start dots {^ synt-roles subject} dots pre-adverb dots
               process dots post-adverb dots {^ synt-roles iobject} dots
               {^ synt-roles object} dots ;particle dots
               {^ synt-roles subj-comp} {^ synt-roles obj-comp} dots
               {^ synt-roles by-obj} {^ synt-roles dative} stop-kernel dots)))
      ((verb-fronted yes)
           (pattern
              (dots start dots pre-adverb dots process dots post-adverb dots
               {^ synt-roles subject} dots {^ synt-roles iobject} dots
               {^ synt-roles object} dots ;particle dots
               {^ synt-roles subj-comp} {^ synt-roles obj-comp} dots
               {^ synt-roles by-obj} {^ synt-roles dative} stop-kernel dots)))))


  ;; Case assignment
  ;; Subject of present-participle mood can be possessive mood.
  (alt synt-role-case
    (;; JR-added 1/28/93 to allow partic-less material clauses (e.g., Go!).
     ((synt-roles none))
     ((synt-roles given)
      (synt-roles ((alt (:wait {^ subject syntax case})
                     (((subject ((syntax ((case subjective))))))
                      ((subject ((syntax ((case given))))))))
                   ;; CBC - Version 2.3: Added reflexive case
                   (opt ((object  ((syntax ((case ((alt (objective reflexive))))))))))
                   (opt ((iobject ((syntax ((case objective)))))))))))))


;; ============================================================
;; SUBCATEGORIZATION
;; ============================================================

(def-conj subcat
  ;; Get lexical information on government pattern
  (process ((subcat {^2 oblique})))
  ;; special treatment of subject clauses (this is the right time
  ;; because the subject is now bound).
  (:! subject-mood)
  (:! subject-subcat)
  (:! object-subcat)
  (:& subj-comp-cat)
  (:& obj-comp-cat)
  (:! by-obj-cat)
  (synt-roles ((:! dative-cat)))
)


(def-alt subject-subcat
  ;; Syntactic category of subject is compatible with verb?
  ;; This depends on particular verb, information we will get from the
  ;; lexicon. So far, we check whether subject-clause and object-clause
  ;; are ok.
  ;; SUBJECT CAT = NP, CLAUSE.
  (((synt-roles ((subject none))))
   ((alt subject-subcat2 (:index (synt-roles subject cat))

;; JR-11-17-92: commented out the wait
      (:wait (({^ synt-roles subject cat} #(under lexical-cat))))

      (((synt-roles ((subject none))))
       ((synt-roles ((subject ((cat np))))))
       ;;*** CBC 9/21/03
       ;; The following allows focused consitutents which are PP's,
       ;; such as "Amidst the enthusiasm was lots of caution."
       ((synt-roles ((subject ((cat pp))))))
       ((synt-roles ((subject ((cat #(under clause))))))
        (alt subject-clause (:index (process subject-clause))
          (:demo "For clausal subjects, what type of clause must be used?")
          (((process ((subject-clause infinitive)))
            (synt-roles ((subject ((mood {^3 process subject-clause}))))))
           ;;*** YD don't ask for (subject-clause #(under present-participle))
           ;; 1/10/98
           ((process ((subject-clause present-participle)))
            (synt-roles ((subject ((mood {^3 process subject-clause}))))))  
           ((process ((subject-clause that)))
            (synt-roles ((subject ((mood bound-nominal))))))
           ((process ((subject-clause that)))
            (synt-roles ((subject ((mood wh-nominal))))))
           ((synt-roles ((subject ((cat #(under list))))))))))))
    (opt ((synt-roles ((subject ((synt-funct subject))))))))))


(def-alt object-subcat
  ;; OBJECT CAT = NP, CLAUSE, PP? (deal with?)
  (((synt-roles ((object none))))
   ((alt object-subcat1 (:index  (synt-roles object cat))

;; JR-11-17-92: commented out the wait
       (:wait (({^ synt-roles object cat} #(under lexical-cat))))

      (((synt-roles ((object none))))
       ((synt-roles ((object ((cat np))))))
       ((synt-roles ((object ((cat pp))))))
       ((synt-roles ((object ((cat #(under clause))))))
    (alt object-clause (:index (process object-clause))
      (:demo "For clausal objects, what type of clause must be used?")
      (((process ((object-clause infinitive)))
        (synt-roles ((object ((mood infinitive))))))
       ((process ((object-clause #(under present-participle))))
        (synt-roles ((object ((mood present-participle))))))
       ((process ((object-clause that)))
        (synt-roles ((object ((mood bound-nominal))))))
       ((process ((object-clause that))) ;***
        (synt-roles ((object ((mood wh-nominal))))))
       ((process ((object-clause wh)))
        (synt-roles ((object ((mood wh)))))))))
       ;;*** CBC - Version 2.3: Reflexive objects
       ((process ((reflexive yes)))
        (synt-roles ((object ((cat personal-pronoun) (gender {^3 synt-roles subject gender})
                              (number {^3 synt-roles subject number})
                              (case reflexive)
                              (person {^ 3 synt-roles subject person}))))))
       ((synt-roles ((object ((cat #(under list)))))))))
    (opt ((synt-roles ((object ((synt-funct object))))))))))


(def-conj subj-comp-cat
  ;; SUBJ-COMP CAT = NP, AP, PP, ADV,
  ;; + ADDRESS, DATE JR-11-21-92
  ;; + participles JR-1-30-93
  ;; + phrases JR-3-8-95
  (synt-roles ((subj-comp ((alt subj-comp-cat1 (:index cat)
                (:wait (({^ cat} #(under lexical-cat))))
                (none
                 ((cat ap))
                 ((cat #(under np)))
                 ((cat #(under pp)))
                 ((cat #(under list)))
                 ;; JR-1-30-93: added
                 ((cat #(under verb))
                  (ending ((alt (#(under past-participle)
                                 #(under present-participle))))))
                 ;; JR-11-21-92: added
                 ((cat #(under address)))
                 ((cat #(under date)))
                 ;; CBC - Version 2.3: Added absolute time as a subject complement category
                 ((cat #(under absolute-time)))
                 ((cat #(under phrase)))
                 ((cat #(under adv)))))
               (opt ((synt-funct subj-comp))))))))


(def-conj obj-comp-cat
  ;; OBJ-COMP CAT = NP, AP, PP, ADV,
  ;; + ADDRESS, DATE JR-11-21-92
  ;; + participles JR-1-30-93
  ;; + phrases JR-3-8-95
  (synt-roles ((obj-comp ((alt obj-comp-cat1 (:index cat)
                   (:wait (({^ cat} #(under lexical-cat))))
                   (none
                ((cat ap))
                ((cat #(under np)))
                ((cat #(under pp)))
                ;; CBC - Version 2.3: Added clause due to Penn TreeBank examples
                ((cat #(under clause)))
                ((cat #(under list)))
                ;; JR-1-30-93: added
                ((cat #(under verb))
                 (ending ((alt (#(under past-participle)
                                #(under present-participle))))))
                ;; JR-11-21-92:
                ((cat #(under address)))
                ((cat #(under date)))
                ;; CBC - Version 2.3: Added absolute time as a subject complement category
                ((cat #(under absolute-time)))
                ((cat #(under phrase)))
                ((cat #(under adv)))))
              (opt ((synt-funct obj-comp))))))))


(def-alt by-obj-cat
  ;; BY-OBJ CAT = PP, set prep
  (((synt-roles ((by-obj none))))
   ((synt-roles ((alt by-obj-cat1 (:index (by-obj cat))
           (((by-obj none))
            ((by-obj given)
             (by-obj ((cat pp)
                      (synt-funct by-obj)
                      ;; Prep set in alt agentless - no need here
                      ;; (prep ((lex "by")))
                      (np ((alt (((cat np)
                                  (syntax ((case objective))))
                                 ;; CBC - Version 2.3: Added lists as a by-obj
                                 ((cat np)
                                  (syntax ((case reflexive))))
                                 ((cat list))))))))))))))))

(def-alt dative-cat
  ;; DATIVE CAT = PP, set prep
  (((dative none))
   ((dative given)
    (dative ((cat pp)
         ({^ ^ process dative-prep} given)
         (prep ((lex {^4 process dative-prep})))
         (np ((cat np)
          (syntax ((case objective))))))))
   ((dative ((cat pp)
         (prep ((lex "to")))
         (synt-funct dative)
         (np ((cat np)
          (syntax ((case objective))))))))))

;; CBC - Version 2.3: Added interjections, addressees, responses
(def-alt interjections
  (((interjection none))
   ((interjection ((cat interjection) (lex given) (emphatic no)
                   (punctuation ((after ",") (capitalize yes))))))
   ((interjection ((cat interjection) (lex given) (emphatic yes)
                   (punctuation ((after "!") (capitalize yes))))))))

(def-alt addressee
  (((addressee none))
   ((addressee ((cat np) (position front) (punctuation ((after ","))))))
   ((addressee ((cat np) (position end) (punctuation ((before ","))))))))

(def-alt response
  (((response none))
   ((response ((cat adv) (punctuation ((after ","))))))))

;; ============================================================
(provide "clause")
;; ============================================================
