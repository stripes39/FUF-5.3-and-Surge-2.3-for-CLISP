;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:  -*-
;;; -----------------------------------------------------------------------
;;; File:         mood.l
;;; Description:  New mood system for SURGE
;;; Author:       Michael Elhadad & Jacques Robin
;;; Created:      27 Nov 1992
;;; Modified:      5 Jul 1995 SURGE 2.2 VERSION
;;;                           - Add generic-mood to deal with same-mood constraint
;;;                             in clause conjunction
;;;                           - Allow innermost-role to be none (no partic).
;;;                           - Allow binders for non-finite except imperative
;;;                           - Add possessive-relative "The man whose car I wash"
;;;                5 Nov 1995 - Fix rule of which/that/who for relative-marker
;;;               12 May 1995 - Fix alt infinitive for bare-infinitive...
;;;                             Fix alt subject-mood for bare-infinitive...
;;;               19 Aug 1996 - Remove (dative-move no) to (mood relative)
;;;                             to allow relative clauses of lexical
;;;                             processes of 3 roles.
;;;               25 May 2005 SURGE 2.3 VERSION
;;;                           - Added question-mode feature for direct/indirect
;;;                           - Added a feature for addressees in dialogue
;;;                           - Similarly, a feature for interjections in dialogue
;;;                           - Expanded the range of binder syntactic types
;;;                           - Support for gapping the "to" and "for" in infinitives
;;;                           - Scoped questions based on APs are marked with "how"
;;; -----------------------------------------------------------------------

;(in-package :FUG5)

(def-alt mood
  ;; MOOD SYSTEM
  ;; For relative and interrogative, the mapping scope to role is
  ;; delayed after the transitivity system (same time as the voice
  ;; system does the mapping semantic/syntactic roles).
  ;; In this first system, all pre-selections that can be derived from
  ;; the mood are set.
  (:index mood)
  (:demo "Deciding between mood finite and non-finite")

  (((mood finite)
    (alt finite (:index mood)
      (:demo "Is the clause declarative, interrogative, relative or subordinate?")
      (((mood declarative)
        (generic-mood declarative)
        (pattern ({^ headers 1} {^ headers 2} stop-header dots start dots)))

       ((mood interrogative)
        (generic-mood interrogative)
        ;; for both yes-no and wh questions, front the tensed part
        ;; of the verb group and the not particle.
        ;; copy everything from tensed-feature except the gap
        ;; (therefore cannot just conflate them).
        ;; Note: these are all the features known to the morphology.
        ;; CBC - Version 2.3: Gap the scoped constitutent only if not one of these process types
        ;; I can't remember what surface forms were these needed for -- perhaps binders?
        (alt gap-unless-ascriptive
           (;((proc ((type material))))
            ;((proc ((type possessive))))
            ;((proc ((type mental))))
            ;((proc ((type ascriptive))))
            ((scope ((gap yes))))))
        ;; For wh questions don't use dative-move
        (alt (((mood wh)
               ;; CBC - Version 2.3: Added a feature for direct and indirect questions
               (alt (((question-mode direct))
                     ((question-mode indirect))))
               (dative-move no))
              ((mood yes-no))))
        (process ((interrogative {^2 mood})))
        (cset ((- fronted-aux)))
        (alt (((question-mode direct))
              ((question-mode indirect))))
        (alt AUX (:wait {^ verb tensed-feature})
           ;; CBC - Version 2.3: Indirect questions have no fronted auxiliary
           (((question-mode indirect)
             (process ((question-mode indirect)))
             (fronted-aux none)
             (process ((tensed-feature {^2 process event}))))
            ((question-mode direct)
             (process ((question-mode direct)))
             (fronted-aux
                ((person {^2 verb tensed-feature person})
                 (number {^2 verb tensed-feature number})
                 (ending {^2 verb tensed-feature ending})
                 (tense  {^2 verb tensed-feature tense })
                 (cat    {^2 verb tensed-feature cat})
                 (lex    {^2 verb tensed-feature lex})))))))


       ;; ***** Missing here: wh-nominal  (as an interrogative, not a bound entity)
       ;; "I know WHAT LOVE IS."


       ((mood bound)
    ;; For conjunctions like:
    ;; I know who he is and whether he is coming.
    (generic-mood declarative)
    ;; CBC - Version 2.3: Added a feature for addressees in dialogue
    ;;                    e.g.:  "John, go get my coat."
    ;;                    And another for interjections
    ;;                    e.g.:  "Wow, Mary, your cooking is great!"
    (alt (((addressee none)
           (pattern (stop-header dots interjection dots front-adverbial-1 dots
                     binder dots start dots)))
          ((addressee ((position front)))
           (pattern (stop-header dots interjection dots response dots addressee dots
                     front-adverbial-1 dots binder dots start dots)))
          ((addressee ((position end)))
           (pattern (stop-header dots interjection dots response dots addressee dots
                     front-adverbial-1 dots binder dots start dots)))))
    ;; CBC - Version 2.3: Expand the types of binders allowed, due to examples
    ;;                    found in the Penn TreeBank, often involving time
    ;;                    expressions like "the day"
    (alt allowable-binders
      (((binder ((cat conj))))
       ((binder ((cat common))))
       ((binder ((cat adv))))
       ((binder ((cat ap))))))
    (alt bound-moods (:index mood)
      (((mood bound-nominal)
        (alt the-binder
          (((binder ((lex ((alt (given "that" "whether" "if")))))))
           ((binder ((cat common))))
           ((binder ((cat ap))))))
        ;; If clause is scoped by a long distance dependency, don't use
        ;; binder: the person who you think [that] won the prize.
        (alt bound-scoped (:index scoped) (:wait scoped)
         (((scoped no))
          ((scoped #(under yes))
           (binder ((gap yes))))))
        (alt mood-bound-nominal (:index mood)
          (((mood bound-nominal-declarative))
           ((mood bound-nominal-subjunctive)
            (modality none)
            (epistemic-modality none)
            (deontic-modality none)
            (process ((ending root)))))))

       ;; Added by JR-1/18/93
       ((mood bound-adverbial)))))

       ;; relative -- mapping scope/role is done in voice system.
       ;; Just indicate here that scope has a gap.
       ((mood relative)
    (generic-mood relative)
    ;; JR-note: origin of the dgsB1c failure
    ;; (dative-move no)
    (scope ((gap yes)))))))

   ((mood non-finite)
    (modality none)
    (epistemic-modality none)
    (deontic-modality none)
    (alt non-finite-moods (:index mood)
      (((mood imperative)
        (generic-mood imperative)
        (process ((ending root) (voice active)))
        (innermost-role ((alt (none ((gap yes))))))
        ;; CBC - Version 2.3: Added a feature for addressees in dialogue
        ;;                    e.g.:  "John, go get my coat."
        ;;                    And another for interjections
        ;;                    e.g.:  "Wow, Mary, your cooking is great!"
        (alt (((addressee none)
               (pattern (stop-header dots binder dots start dots)))
              ((addressee ((position front)))
               (pattern (stop-header dots addressee dots binder dots start dots)))
              ((addressee ((position end)))
               (pattern (stop-header dots binder dots start dots addressee dots))))))
       ((alt (((addressee none))
              ((addressee ((position front)))
               (pattern (stop-header dots interjection dots response dots
                         addressee dots binder dots start dots)))
              ((addressee ((position end)))
               (pattern (stop-header dots interjection dots response dots
                         binder dots start dots addressee dots)))))
        ;; - All cases can have a binder:
        ;;   "BEFORE JUMPING, check your watch."
        ;;   "We must discover HOW TO DO IT."
        ;;   "AS SOON AS KOED BY IRON MIKE, he found himself a millionaire."
        ;; - All cases can be conjoined together:
        ;;   "The box accessible to all, burned by the sun and bulging under cover."
        ;;   "The man crushed by love, now knowing what love is and happy about it."
        (generic-mood embedded-mood)
        ;; CBC - Version 2.3: Added a new binder category
        (alt non-finite-binder (((binder none))
                                ((binder ((cat conj))))
                                ((binder ((cat adv))))))
        (alt embedded-moods (:index mood)
           (((mood present-participle)
             (process ((ending present-participle))))

            ((mood past-participle)
             (process ((ending past-participle)
                       (tensed-feature ((gap yes)))
                       (voice passive))))

            ((mood verbless)
             (process ((type ascriptive) (gap yes))))

       ;; Bare-infinitive cf. Quirk p.993
       ;; "RATHER THAN SHOOT, Bo made the perfect pass."
       ((mood #(under bare-infinitive))
        (process ((ending root))))

       ;; CBC - Version 2.3: Added new infinitive possibilities, such as
       ;;                    possibility to gap the "to" and the "for"
       ((mood infinitive)
        (process ((ending infinitive)
                  (gapped-infinitive ((alt (given no))))))
        (alt infinitive
         ;; yael add under for-to
         (((mood #(under for-to-infinitive))
           (pattern (stop-header dots for start dots))
           (alt (((for ((cat prep) (lex "for"))))
                 ((for ((gap yes)))))))
          ;; Any other subclass of infinitive
          ((mood infinitive)))))))))))))


(def-alt subject-mood
  (:demo "Is a subject required or does it need a special treatment?")
  (
   ;; Moods w/ obligatory subjects
   ((mood ((alt (declarative yes-no bound relative))))
    (synt-roles ((subject given))))

   ;; Moods w/ possibly absent subject due to question scoping
   ((mood wh)
    (alt scoped-subject (((synt-roles ((subject given))))
             ((scope {^ synt-roles subject})))))

   ;; Moods w/ possibly absent subject due to control by embedding clause
   ((mood ((alt (past-participle verbless))))
    (alt controlled-subject (((synt-roles ((subject given)))
                  (controlled none))
                 ((controlled given)
                  (controlled ((gap yes)))
                  (controlled {^ synt-roles subject})))))

   ((mood present-participle)
    (alt present-participle-subject
      (
       ;; For nominal functions, subject present ==> possessive (or objective) case
       ((synt-roles ((subject given)))
    (controlled none)
    (synt-funct #(under synt-role))
    (synt-roles
     ((subject ((cat np)
            (gap none)
            (syntax ((case ((alt (possessive objective)))))))))))
       ((synt-roles ((subject given)))
    (controlled none))

       ;; If subject absent it is controlled by embedding clause
       ((controlled given)
    (controlled ((gap yes)))
    (controlled {^ synt-roles subject})))))

   ((mood to-infinitive)
    ;; Allows only controlled subjects
    (alt to-infinitive-controlled-subject
    (((synt-roles ((subject given)))
      (synt-roles ((subject ((gap yes)))))
      (controlled {^ synt-roles subject}))
     ((synt-roles ((subject none)))))))

   ((mood bare-infinitive)
    ;; Allows only controlled subjects
    (alt bare-infinitive-controlled-subject
    (((synt-roles ((subject given)))
      (synt-roles ((subject ((gap yes)))))
      (controlled {^ synt-roles subject}))
     ((synt-roles ((subject none)))))))

   ((mood for-to-infinitive)
    ;; Requires present and ungaped subject (in objective case)
    (synt-roles ((subject given)
         (subject ((gap none)
               (case objective))))))

   ((mood imperative)
    ;; Subject absent or gapped
    (synt-roles ((alt imperative-subject (((subject none))
                      ((subject ((gap yes)))))))))))



;; ============================================================
;; DISPLACED CONSTITUENT
;; ============================================================

(def-alt displaced-constituent (:index mood)
  ;; This wait is necessary for long distance dependencies: you need to
  ;; wait until the scoped constituent is mapped in the embedded clauses
  ;; before processing it.
  (:wait {^ scope synt-funct})
  ;; CBC - Version 2.3: Here and below, support for indirect questions
  (((mood yes-no) (question-mode direct)
    ;; No displaced component except inversion of verb/subject
    ;; Adverb remains in regular position (unlike notf which is fronted).

    (pattern (stop-header dots fronted-aux fronted-not start dots)))

   ((mood wh)
    ;; Find pointer to scope realization - this is scoper.
    (alt (((question-mode direct))
          ((question-mode indirect))))
    (alt (((scope ((synt-funct given)))
           (scoper {^ scope}))
          ((scope ((realization head)))
           (scoper {^ scope}))
          ((scoper {^ scope realization}))))
           (scoper ((gap yes)
                    (clause-level ((scoped yes)))
                    (question-pronoun {^2 scope question-pronoun})
                    (question-embedded {^2 scope question-embedded})))
    (:! question-embedded)

    (alt (((question-mode indirect)
           (pattern (stop-header dots question dots fronted-adverb dots start dots
                     {^ SYNT-ROLES SUBJECT} dots process dots fronted-aux dots
                     fronted-not dots)))
          ((question-mode direct)
           (pattern (stop-header dots question fronted-adverb
                     fronted-aux fronted-not dots start
                     {^ SYNT-ROLES SUBJECT} dots))))))

   ;; MOOD RELATIVE
   ((mood relative)
    ;; Find pointer to scope realization - this is scoper.
    (alt (((scope ((synt-funct given)))
       (scoper {^ scope}))
      ((scope ((realization head)))
       (scoper {^ scope}))
      ((scoper {^ scope realization}))))
;;    (cset ((- scope scoper)))
    (scoper ((gap yes)
         (clause-level ((scoped yes)))
         (relative-marker {^2 scope relative-marker})
         (relative-embedded {^2 scope relative-embedded})))

    (pattern (stop-header relative-marker dots start dots))

    (:! relative))

   ;; OTHER MOODS -- Nothing to do here
   ((mood declarative))
   ((mood bound))
   ((mood non-finite))))



(def-alt question-embedded (:index (scope question-embedded))
  ;; scope is a path to a semantic constituent on which the question is asked.
  ;; We expect to find in the semantic constituent the realization link to
  ;; the syntactic complement realizing it plus the following information:
  ;; question-embedded yes/no which determines whether the question is a
  ;; pronoun or a prep pronoun.
  ;; In the syntactic const. we find the prep to use for embedded questions
  ;; and the question-pronoun to use.
  ;; Default for embedded is no.
  ;; ***** Should check for other cats (adverb, ap).
  ;; ***** Should check features of verb to decide whether
  ;; ***** to use a pp or an np as question element
  ;; ***** Should do questions like "to which house did you
  ;; ***** go?" (long distance - scope is sub-constituent)
  ;; The features needed to determine a relative or question pronoun are:
  ;; case, restrictive, animate, possessive.
  ;; We pass them explicitly so that we do not depend on the cat affected
  ;; to the scoped constituent.

  ;; CBC - Version 2.3: Make sure scoped APs are handled correctly
  (((scope ((cat given) (cat ap)))
    (question ((cat ap)
               (lex {^ ^ scoper lex})
               (head {^ ^ scoper head})
               (classifier {^ ^ scoper classifier})
               (qualifier {^ ^ scoper qualifier})
               (modifier ((cat adv) (lex "how"))))))
   ((scope ((question-embedded no)))
    (question {^ scoper question-pronoun})
    (question ((cat question-pronoun)
               (restrictive {^2 scope restrictive})
               (animate {^2 scope animate})
               (possessive {^2 scope possessive})
               (syntax {^2 scoper syntax})
               (semantics ((index {^3 scoper semantics index})))))
    (alt (((question ((lex given)))
           (cset ((- question))))
          ((cset ((+ question)))))))
   ((scope ((question-embedded yes)))
    (cset ((+ question)))
    (question ((cat pp)
           (alt (((prep {^2 scope question-prep})
              (prep given))
             ((prep {^2 scope prep})
              (prep given))
             ((prep {^2 scoper prep})
              (prep given))
             ((prep ((lex "of"))))))
           (np {^2 scoper question-pronoun})
           (np ((cat question-pronoun)
            (restrictive {^3 restrictive})
            (restrictive {^3 scope restrictive})
            (animate {^3 scope animate})
            (possessive {^3 scope possessive})
            (syntax ((case objective)))
            (semantics {^3 scoper np semantics})))
           ;; If lex for pronoun is already given don't bother with it.
           (alt (((np ((lex given)))
              (cset ((- np prep))))
             ((cset ((+ np) (- prep)))))))))))



(def-alt relative (:index (scoper relative-embedded))
  (:demo "Is the relative clause simple or embedded in a PP?")
  (:wait (({^ scope cat} #(under lexical-cat))))
  (((mood simple-relative)
    ;; Example: the woman who lives there
    ;;          the man whom I know
    ;;          the reason why I came
    ;; Simple relative is the qualifier of an NP. The NP
    ;; is a constituent of the relative clause, as indicated
    ;; by the scope constituent:
    ;; if NP is medium, do (scope ((role medium))) in the relative
    ;; clause. Scope inherits the relevant features from the
    ;; head of the enclosing NP.

    (scoper ((gap yes)
         (relative-embedded no)
         (cat simple-np)
         (lex {^3 head lex})
         (semantics ((index {^4 semantics index})))))
    (relative-marker {^ scoper relative-marker})
    (relative-marker ((cat relative-pronoun)
              (restrictive {^2 scope restrictive})
              (animate {^2 scope animate})
              (possessive {^2 scope possessive})
              (restrictive {^2 restrictive})
              (syntax {^2 scoper syntax})
              (semantics {^2 scoper semantics})))
    (alt (((relative-marker ((lex given)))
           (cset ((- relative-marker))))
           ;; Rule (Winograd p.479 B.1.4)
           ;; Stylistic rule: restrictive pronoun -> that vs. which
           ;; for animate, keep who in any case.
          ((relative-marker ((restrictive #(under yes))
                             (animate no)
                             (lex "that")))
           (cset ((- relative-marker))))
          ((cset ((+ relative-marker)))))))

   ;; possessive relative - scope is NOT determiner of an NP
   ;; it is the whole NP because you need to displace the whole thing around:
   ;; Ex: "The man WHOSE CAR I WASH [trace]."
   ;; If you had trace on {affected possessor} and were not defining
   ;; possessive-relative as a separate mood, you would get:
   ;; "The man WHOSE I WASH [trace] CAR."
   ((mood possessive-relative)
    ;; ***** Copying any type of NP in any form (annoying).
    ;; Done here only for common and partitive.
    (alt (((scoper ((cat #(under partitive))))
       (relative-marker ((cat partitive)
                 (part {^2 scoper part})
                 (part-of {^2 scoper part-of})
                 (part-of ((possessor ((cat relative-pronoun)))))
                 (all {^2 scoper all})
                 (prep {^2 scoper prep}))))
      ;; Note: need to process scoper here otw all the features at the
      ;; top level of the NP won't be propagated as needed into head
      ;; and others (syntax...).  So add it to cset.
      ;; Otherwise wouldn't work with:
      ;; "The soil, whose layers are topsoil and subsoil."
      ;; as number wouldn't be processed correctly - credit to Charles
      ;; Brendan Callaway for finding this bug.
      ((scoper ((cat np)))
       (cset ((+ scoper relative-marker)))
       (relative-marker ((cat np)
                 (lex {^2 scoper lex})
                 (head {^2 scoper head})
                 (possessor ((cat relative-pronoun)))
                 (cardinal {^2 scoper cardinal})
                 (ordinal {^2 scoper ordinal})
                 (reference {^2 scoper reference})
                 (semantics {^2 scoper semantics})
                 (syntax {^2 scoper syntax})
                 (describer {^2 scoper describer})
                 (classifier {^2 scoper classifier})
                 (qualifier {^2 scoper qualifier})))))))

   ;; embedded relative - scope is within a PP
   ((mood embedded-relative)
    (scoper ((relative-embedded yes)))
    ;; Example: the box in which we store the elixir
    ;;          an experience the likes of which you have never seen
    ;; Bind scope to embedding np head
    (scoper ((np ((semantics ((index {^5 semantics index})))))))
    (relative-marker ((cat pp)
              (pattern (prep np))
              (prep {^2 scoper prep})
              (np {^2 scoper relative-marker})
              (np ((cat relative-pronoun)
               (syntax ((case objective)))
               (restrictive {^3 scope restrictive})
               (animate {^3 scope animate})
               (possessive {^3 scope possessive})
               (semantics {^3 scoper np semantics})))))
    (alt (((relative-marker ((np ((lex given)))))
       (cset ((- relative-marker))))
      ((cset ((+ relative-marker)))))))

   ;; ***** be-deleted not implemented [From Winograd]
   ;; Actually debatable whether useful: in the meantime, can
   ;; generate them as AP in qualifier (cf note below on
   ;; criteria to distinguish adjectives and participles).
   ;; Example: Goldwater /who was/ crushed by his defeat
   ;;          the team /that is/ expected to win
   ;;          an enchanting island /that is/ waiting to be seen
   ;; NOTE: This is replaced by the following construct:
   ;; qualifier of an NP as:
   ;; ((qualifier ((cat clause)
   ;;              (mood past-participle)
   ;;              (controlled {path to implicit participant filled by head}))))
   #+ignore((mood be-deleted-relative))

   ;; ***** wh-nominal not implemented
   ;; Example: They were amazed at which one they chose
   ;;          I couldn't believe how many people understood
   ;;          What they did next was surprising
   ;; To rework when nominalization is implemented [cf Vendler]
   ;; NOTE: This is replaced by the following construct:
   ;; ((cat clause)
   ;;  (mood bound-nominal)
   ;;  (binder ((lex "what")))
   ;;  ...)
   #+ignore((mood wh-nominal-relative))

   ;; ***** wh-ever-nominal not implemented
   ;; Example: Whoever did it will be sorry
   #+ignore((mood wh-ever-nominal-relative))))

;; ============================================================
(provide "mood")
;; ============================================================
