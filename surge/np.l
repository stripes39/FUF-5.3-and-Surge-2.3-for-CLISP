;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         np.l
;;; Description:  Grammatical systems for NP
;;; Author:       Michael Elhadad & Jacques Robin
;;; Created:      19 Dec 1991
;;; Modified:     12 May 1992: allowed head of common to be an embedded NP.
;;;               19 Aug 1992: merged in expanded NP (JR).
;;;               24 Jan 1993: removed fsets from top-level nominal cats (JR)
;;;                            added adverbial functions to np-functions (JR)
;;;               23 Mar 1993: added qualifying PPs in measures (JR)
;;;               28 Jun 1993: added commas before restrictive quals. (ME)
;;;               05 Dec 1993: added , after restrictive quals. (ME)
;;;               06 Dec 1993: changed partitive to gap head of part when
;;;                            not specified. (ME)
;;;               21 Dec 1993: defined np-propagate. (ME)
;;;                5 Jul 1995: SURGE 2.2 VERSION
;;;                            - Added synt-funct qualifier to qualifiers
;;;                              to avoid confusion with main-clauses.
;;;                5 Nov 1995: moved person-name to special.l
;;;                            made address and date as np-types.
;;;                3 JUN 1996: alt np-case: - possessive feature under NP
;;;                            and not under HEAD (YD)
;;;               10 Sep 1998: add (cat det) to determiner in common-head
;;;               (YD)
;;;               01 Oct 1998: YD add (restrictive yes) for compound proper qualifiers.
;;;               25 May 2005: SURGE 2.3 VERSION
;;;                            - Added adverbial modifiers for partitives
;;;                            - Added numeric ranges
;;;                            - Added three features:  anaphoric, own and acronym
;;;                              which can be propagated through the NP
;;;                            - Propagate informality through relative clauses to head
;;;                            - Allow describer/qualifier/classifier for propers
;;;                            - Increased possible classifier categories
;;;                            - Initial attempt at appositives
;;; -----------------------------------------------------------------------

;(in-package :FUG5)

(def-conj trivial-proper
  ;; A branch of the grammar for proper nouns that are already lexicalized
  ;; and do not want any of the expensive np processing.
  ;; ***** Do defaults for number and other features...
  (cat #(under trivial-proper))
  (pattern (head))
  (head ((lex {^2 lex})
         (cat noun))))


;; List of synt-funct that an NP can fill - except classifier!
(def-alt np-functions
  (head
   #(under subject)
   #(under object)
   #(under subj-comp)
   #(under iobject)
   #(under dative)
   #(under obj-comp)
   #(under by-obj)
   #(under pred-adjunct)
   #(under sent-adjunct)
   #(under disjunct)))


(def-conj partitive
  ;; A sort of NP of the form: a cup of tea, 12 of 13 3 point shots.
  ;; Constituents are: part, part-of, prep, total
  (cat #(under partitive))
  ;; CBC - Version 2.3: Added modifier to pattern to allow new
  ;;                    forms like "Almost all the slices of the pie"
  (pattern (modifier all part prep part-of))
  ;;(fset (all total part prep part-of modifier
  ;;     cat pattern sss synt-funct syntax clause-level semantics))
  (total {^ part-of partitive-total})
  (alt all-partitive (:index all)
    (((all none)
      (total none))
     ((total *)
      (all ((cat phrase) (lex "almost"))))
     ((total =)
      (all ((cat phrase) (lex "exactly"))))
     ((total &)
      (all ((cat phrase) (lex "about"))))
     ((total +)
      (all ((cat phrase) (lex "all"))))))
  (prep ((cat prep) (opt ((lex "of")))))
  (part ((alt part-cat (:index cat)
           (((cat cardinal))
            ((cat compound-cardinal))
            ((cat ordinal))
            ((cat compound-ordinal))
            ((cat common)
            ;; Default head is the gap of the head of part-of
             (alt (((head given))
                   ((lex given))
                   ((head ((lex {^3 part-of head lex}) (gap yes)))))))
           ((cat measure))
           ((cat fraction))))))
  (part-of ((alt part-of-cat (:index cat)
          (((cat common))
           ((cat measure))))))
  (synt-funct ((:! np-functions)))
  ;; CBC - Version 2.3: Number agreement for percentage: 23% of <plural>
  (alt (((part ((percent none))))
        ((part ((percent +)))
         (part-of ((number plural))))))
  ;; Number agreement for partitive: all of <plural>
  (opt ((total #(under +))
        (part ((value given)
               (control (and (numberp #@{^ value})
                             (/= #@{^ value} 1)))))
        (part-of ((cat ((alt (#(under common) #(under measure)))))
                       (number plural)))))
  )

;; CBC - Version 2.3: New category of numerical ranges, for phrases like:
;;                    "From 10 to 15 dollars."
(def-conj numeric-range
  (cat #(under numeric-range))
  (cset ((= first-range second-range qualifier)))
  (pattern (dots first-phrase first-range second-phrase second-range qualifier dots))
  (first-range ((cat #(under np))
                (alt (((cardinal given)) ((cardinal none))))
                (opt ((definite no)))
                (opt ((cardinal ((digit yes)))))))
  (second-range ((cat #(under np))
                 (alt (((cardinal given)) ((cardinal none))))
                (opt ((definite no)))
                (opt ((cardinal ((digit yes)))))))
  (head {^ second})
  (alt (((qualifier none))
        ((qualifier ((cat pp))))
        ((qualifier ((cat adv-p))))
        ((qualifier ((cat #(under np)))))))
  (first-phrase ((alt (((cat phrase))
                       ((cat adv))))))
  (second-phrase ((alt (((cat phrase))
                        ((cat adv))))))
  (opt ((first-phrase ((cat phrase) (lex "")))))
  (opt ((second-phrase ((cat phrase) (lex "to"))))))

(def-conj measure
  (cat #(under measure))
  (quantity ((alt quantity-type (((cat cardinal))
                 ((cat ordinal))
                 ((cat compound-cardinal))
                 ((cat compound-ordinal))
                 ((cat fraction))))))
  (unit ((alt (((cat noun))
           ((cat noun-compound))))))

  ;; dots added 4/24/93 to allow split quantity constituent,
  ;; e.g., 2 games in a row at home
  (alt (((qualifier none)
     (pattern (quantity unit dots)))
    ((qualifier ((cat pp) (synt-funct qualifier)))
     (pattern (quantity unit dots qualifier)))))

  ;; Compute number if not specified in input.
  (opt ((alt (((quantity ((cat cardinal)
              (value given)
              (control (and (numberp #@{^ value})
                    (/= #@{^ value} 1)))))
           (number plural))
          ((quantity ((cat compound-cardinal)
              (numeral ((value given)))
              (control (and (numberp #@{^ numeral value})
                    (/= #@{^ numeral value} 1)))))
           (number plural))))))

  ;; Measure used as a classifier does not put an s on unit
  (alt measure-unit-number
    (((synt-funct ((:! np-functions)))
      (unit ((number {^2 number}))))
     ((synt-funct #(under classifier))
      (unit ((number singular))))))
  )


;; Recursively embedded noun compounds
;; Example: "The pleasant (((house property) tax) office) furniture)"
;; (Quirk p.919)
;; This only appears as an NP-head or classifier, not as a whole NP.
(def-conj noun-compound
  (cat #(under noun-compound))
;;  (fset (cat pattern sss number classifier head synt-funct))
  (pattern (classifier head))
  (classifier ((synt-funct classifier)
           (alt (((cat noun))
             ((cat verb)
              (ending ((alt (present-participle past-participle)))))
             ((cat adj))
             ((cat ap))
             ((cat measure))
             ((cat list))
             ;; CBC - Version 2.3: Added the date category
             ((cat date))
             ((cat noun-compound))))))
  (head
   ((alt (((cat noun)
       (number {^2 number}))
      ((cat noun-compound)
       (number {^2 number}))))))
  )


;; Propagate all top-level features in an NP under their appropriate group
;; syntax, semantics, reference and head.
(def-conj np-propagate
  ;; CBC - Version 2.3: Added three features:  anaphoric, own and acronym
  (syntax ((fset (animate gender case definite person partitive context
              number a-an distance countable anaphoric own acronym))
       (animate {^2 animate})
       (number {^2 number})
       (gender {^2 gender})
       (case   {^2 case})
       (person {^2 person})
       (definite {^2 definite})
       (a-an {^2 a-an})
       (partitive {^2 partitive})
       (distance {^2 distance})
       (countable {^2 countable})
       (own {^2 own})
       (context {^2 context})
       (acronym {^2 acronym})
       (anaphoric {^2 anaphoric})))
  ;; JR-11-15-92: allowed index at the top-level and then copied it under semantics
  ;; This saves 24 characters per co-reference pointer, making them to fit on a
  ;; single line, e.g.
  ;; ((circum ((partic ((processor ((index {^5 partic processor index}))))))))
  ;; instead of
  ;; ((circum ((partic ((processor ((semantics ((index {^5 partic processor
  ;; semantics index}))))
  (index ((fset (concept animate gender person number reference-number
         denotation countable anaphoric own acronym context))
      (concept {^2 concept})
      (animate {^2 animate})
      (number {^2 number})
      (gender {^2 gender})
      (person {^2 person})
      (denotation {^2 denotation})
      (reference-number {^2 reference-number})
      (anaphoric {^2 anaphoric})
      (own {^2 own})
      (context {^2 context})
      (acronym {^2 acronym})
      (countable {^2 countable})))
  (semantics ((fset (index describer qualifier classifier))
          (index ((fset (concept animate gender person number
                     reference-number denotation anaphoric own acronym
                     countable))
              (concept {^3 concept})
              (animate {^3 animate})
              (number {^3 number})
              (gender {^3 gender})
              (person {^3 person})
              (denotation {^3 denotation})
              (reference-number {^3 reference-number})
              (anaphoric {^3 anaphoric})
              (own {^3 own})
              (acronym {^3 acronym})
              (countable {^3 countable})))
          (describer {^2 describer})
          (qualifier {^2 qualifier})
          (classifier {^2 classifier})))
  (reference ((fset (type total selective possessive interrogative distance
              quantitative exact orientation evaluative status
             degree comparative superlative evaluation))
          (type {^2 reference-type})
          (status {^2 status})
          (degree {^2 degree})
          (total {^2 total})
          (selective {^2 selective})
          (possessive {^2 possessive})
          (interrogative {^2 interrogative})
          (distance {^2 distance})
          (quantitative {^2 quantitative})
          (exact {^2 exact})
          (comparative {^2 comparative})
          (superlative {^2 superlative})
          (evaluation {^2 evaluation})
          (orientation {^2 orientation})
          (evaluative {^2 evaluative})))
  (head ((lex {^2 lex})
          (definite {^2 definite}))))


;; BRANCH FOR SIMPLE-NP AND ITS SPECIALIZATIONS
(def-conj simple-np
  ;; Prototypical sequence: determiner modifiers head qualifiers
  ;; determiner = (pre-determiner determiner ordinal cardinal)
  ;; modifiers  = (describer classifier)
  ;; qualifiers = (restrictive non-restrictive possessive-marker)
  ;; We expect in the input at the top-level constituents:
  ;; - definite yes/no (default is yes).
  ;; - a lex that will be interpreted as the lex of the head.
  ;; - describer: an embedded list of describers.
  ;; - classifier: an embedded list of classifiers.
  ;; - qualifiers: an embedded list of qualifiers.

  (cat #(under np))
  (complex none)
  ;; GENERAL NP =================================================
  ;; General structure: head, syntax, semantics, determiner.
  (pattern (dots head dots))
  (generic-cat np)

  (:& np-propagate)

  ;; Distance is not always propagated to determiner
  ;; If lex is already specified, don't do anything.
  (alt np-determiner
       (((determiner ((lex given)))
         (cset ((- determiner))))
        ((determiner ((cat det)
              (definite  {^2 syntax definite})
              (countable {^2 syntax countable})
              (anaphoric {^2 syntax anaphoric})
              (own {^2 syntax own})
              (acronym {^2 syntax acronym})
              (number    {^2 syntax number})
              (quantitative {^2 reference quantitative})
              (status {^2 reference status})
              (degree {^2 reference degree})
              (exact {^2 reference exact})
              (comparative {^2 reference comparative})
              (superlative {^2 reference superlative})
              (evaluative {^2 reference evaluative})
              (evaluation {^2 reference evaluation})
              (total     {^2 reference total})
              (selective {^2 reference selective})
              (possessive {^2 reference possessive})
              (interrogative {^2 reference interrogative})
              (orientation {^2 reference orientation})
              (head-denotation {^2 semantics index denotation})
              (reference-number {^2 semantics index reference-number})
              (partitive {^2 partitive}))))
        ((determiner none))))

  (:! np-type)

  (:& np-number)

  (:& np-pre-det)

  ;; DETERMINER
  ;; If possible propagate distance to determiner
  (opt det-distance ((determiner ((distance {^2 syntax distance})))))
  ;; Check if possessor is defined here - it becomes the determiner
  (alt possessor (:index possessor)
    (((possessor given)
      (possessive yes)
      (possessor ((syntax ((case possessive)))))
      ;; JR: opt added
      (opt ((determiner ((possessor {^2 possessor})
             (possessive yes))))))
     ((possessor none)
      (possessive no))
     ((possessor nil)
      (cset ((- possessor)))
      (possessive #(under yes)))))

  (semantics ((:! describer)))

  (semantics ((:! classifier)))

  (semantics ((:! qualifier)))

  (:! np-case))



(def-alt np-type (:index cat)
  (:demo "Is this a common noun, a pronoun or a proper noun?")

  ;; COMMON NOUNS -------------------------------------------
  (((cat common)
    (np-type common)
    (alt common-determiner
     (((determiner ((head-cat common)))
       ;;YD
       (determiner ((cat det)))
       (pattern (determiner dots)))
      ((determiner none))))
    (head ((cat np-head)
       (synt-funct head)
       (a-an {^2 syntax a-an})
       (acronym {^2 syntax acronym})
      ;; Single head or conjunction of nouns?
       (alt common-head (:index cat)
         ((;(lex given)
           (cat noun)
           ;; these only accepted by morphology.
           ;; (fset (cat generic-cat lex number a-an feature semr kind))
           (number {^2 syntax number})
           )
          ((gap given))
          ;; For measure and partitive, no agreement number head/np
          ;; Allow for "a season high 27 points."
          ;; Allow for "a season high 47 of 53 free throws."
          ;; Allow for "Six assignments IS a lot."
          ((cat #(under measure))
           ({^}
        ((alt measure-head-det
             (:demo "Does an NP with a measure as head take a det?")
           (((classifier given)
             (determiner ((head-cat common))))
            ((classifier none)
             (possessor given)
             (determiner ((head-cat common))))
            ((classifier none)
             (possessor none)
             (determiner none)))))))
          ((cat #(under partitive)))
          ((cat #(under noun-compound))
           (number {^2 syntax number}))
          ((complex given)
           ;; to make morphology understand this is not a
           ;; simple noun.
           (cat np-head)
           (number {^2 syntax number})
           (common ((cat noun))))))))

    (person third)
    (alt definite (:wait definite)
      (((definite yes))
       ((definite #(under no)))))
    (countable ((alt (yes #(under no))))))


   ;; PRONOUNS ------------------------------------------------
   ((cat pronoun)
    (np-type pronoun)
    ;; pronouns allow no classifier.
    ;; all except quantified have no describer.
    ;; can have qualifiers
    (semantics ((classifier none)))
    (alt pronoun-determiner
       (((determiner ((head-cat pronoun)))
         (pattern (determiner head dots)))
        ((determiner none)
         (pattern (head dots)))))
    (cset ((- head)))        ;; do not recurse on head
    (head ((cat pronoun)     ;; known to morphology plus its args.
           ;; (fset (cat pronoun-type case gender animate feature semr kind
           ;;        syntax lex number distance person restrictive))
           (gender {^2 syntax gender})
           (number {^2 syntax number})
           (animate {^2 syntax animate})
           (anaphoric {^2 syntax anaphoric})
           (own {^2 syntax own})
           (acronym {^2 syntax acronym})
           (pronoun-type {^2 pronoun-type})
           (distance {^2 syntax distance})
           (person {^2 syntax person})
           (restrictive {^2 restrictive})))
    ;; can have determiner put after head: they all, them all.
    ;; Check in determiner.l how the new order is enforced.
    (pattern (determiner head dots))
    (:! pronoun-type)
    ;; Case: subjective, objective, possessive, reflexive
    (syntax
     ((alt pronoun-case
      (:demo "Is the pronoun subject, object, possessive or reflexive?")
    (((case subjective))
     ((case given)
      (case ((alt (objective possessive reflexive)))))))))
    ;; Propagate case from np to head only if not partitive
    (syntax
     ((alt partitive-case (:index partitive) (:wait partitive)
    (((partitive no)
      (case {^2 head case}))
     ((partitive yes)
      ({^ head case} objective)))))))


   ;; PROPER NOUNS -------------------------------------------
   ((cat proper)
    (np-type proper)
    (syntax ((person third)
             ;; CBC - Version 2.3: Added indefinite propers, as PTB contains some
             (alt (((definite yes)) ((definite no))))))
    (:! proper-type))))



(def-alt pronoun-type
  (:index cat)
  (:demo "Is this a personal, demonstrative, question or quantified pronoun?")
  ;; Pronoun-type: personal, demonstrative, question, quantified
  (((cat personal-pronoun)
    (np-type personal-pronoun)
    ;; are gender and person specific, default is third masculine.
    (head ((pronoun-type personal)))  ;; arg to morph.
    ;; CBC - Version 2.3: Propagate formality to the head
    (alt (((formality formal) (head ((formality formal))))
          ((formality given) (formality informal) (head ((formality informal))))))
    (syntax ((alt (:index person) (:wait person)
                (((person third))
                 ((person #(under first)) (animate yes))
                 ((person #(under second)) (animate yes))))
             (alt gender (:index gender)
                (((gender neuter) (animate no))
                 ((gender masculine) (animate yes))
                 ((gender #(under feminine)) (animate yes))))))
                  (semantics ((describer none))))
   ((cat demonstrative-pronoun)
    (np-type demonstrative-pronoun)
    (head ((pronoun-type demonstrative)))
    ;; distance does not propagate to determiner in this case
    (determiner ((distance none)))
    (syntax ((definite yes)
             (person third)
             (distance ((alt (far #(under near)))))))
    (semantics ((describer none))))
   ((cat relative-pronoun)
    (np-type relative-pronoun)
    (determiner ((distance none)))
    (head ((pronoun-type relative)))
    (syntax ((person third))))
   ((cat question-pronoun)
    (np-type question-pronoun)
    (determiner ((distance none)))
    (head ((pronoun-type question)))
    (syntax ((person third)))
    (semantics ((describer none))))
   ;; - describers come after head: "something green"
   ((cat quantified-pronoun)
    (np-type quantified-pronoun)
    (head ((pronoun-type quantified)))
    (syntax ((person third) (definite no)))
    (alt (((semantics ((describer none))))
          ((semantics ((describer given)))
           (pattern (head describer dots))))))
   ))


(def-alt proper-type (:index cat)
  ;; CBC - Version 2.3: PTB contains examples of modified proper nouns
  ;;                    e.g.:  "the famous Shannon Hotel, in Ireland"
  (((cat basic-proper)
    (alt basic-proper-describer
        (((describer none))
         ((describer given))))
    (alt basic-proper-qualifier
        (((qualifier none))
         ((qualifier given)
          (pattern (dots head qualifier dots)))))
    (alt basic-proper-classifier
        (((classifier none))
         ((classifier given)
          (classifier ((cat ((alt (common proper date)))))))
         ((classifier given)
          (classifier ((cat #(under proper)))))))
    (alt basic-proper-determiner
        (((determiner none)
          (pattern (classifier head qualifier)))
         ((determiner ((head-cat proper)))
          (pattern (determiner dots classifier dots head dots qualifier)))))
    (head ((cat noun)
       ;(lex given)
       ;; Only the ones accepted by morphology + the generic
       ;; (fset (cat generic-cat lex number a-an feature semr kind))
       (number {^2 syntax number})
       (a-an {^2 syntax a-an})
       (acronym {^2 syntax acronym}))))

   ((cat compound-proper)
    ;(classifier none)
    (head ((alt compound-proper-cat (((cat #(under person-name)))
                                     ((cat #(under institution-name)))  ;;Added by Charles
                                     ((cat #(under team-name)))
                                     ((cat #(under np)))
                                     ((cat pp))))
           (animate {^2 animate})
           (number {^2 number})))

    (alt compound-proper-determiner
    (((determiner ((head-cat proper)
               (alt compound-proper-head-premod
               ((({^ describer} none)
                 (head-describer no))
                ((head-describer yes))))))
      (pattern (determiner describer head qualifier)))

     ((determiner none)
      (pattern (describer head qualifier)))))

    (alt compound-proper-qualifier
         (((qualifier given)
           (qualifier ((cat clause)
                       (synt-funct qualifier)
                       ;; YD added (restrictive yes) possibility (ex. t356)
                       (restrictive ((alt (no #(under yes))))))))
          ((qualifier given)
           (qualifier ((cat pp)))
           (pattern (dots head qualifier)))
          ((qualifier none)))))))



(def-conj np-number
  ;; Handle number, cardinal and ordinal for NPs
  ;; Only add cardinal to determiner if det is not none
  (opt ((cardinal given)
        (determiner any)   ;; for not-none
        (determiner ((cardinal {^2 cardinal})))))

  ;; Only add ordinal to determiner if det is not none
  (opt ((ordinal given)
        (determiner any)   ;; for not-none
        (determiner ((ordinal {^2 ordinal})))))

  ;; If cardinal is given, use its value to determine number.
  (alt cardinal-number ;; (:wait {^ cardinal value})
    (((cardinal ((value given)))
      (control (and (numberp #@{^ cardinal value})
                    (= #@{^ cardinal value} 1)))
      (syntax ((number singular))))
     ((cardinal ((numeral ((value given)))))
      (control (and (numberp #@{^ cardinal numeral value})
                    (= #@{^ cardinal numeral value} 1)))
      (syntax ((number singular))))

     ;; any other number is plural
     ((cardinal ((value given)))
      (control (numberp #@{^ cardinal value}))
      (syntax ((number plural))))
     ((cardinal ((numeral ((value given)))))
      (control (numberp #@{^ cardinal numeral value}))
      (syntax ((number plural))))

     ((syntax ((number singular))))
     ((syntax ((number #(under dual)))))
     ((syntax ((number #(under plural))))))))


(def-conj np-pre-det
  ;; Deal with fraction and multiplier
  (alt fraction (:index fraction)
    (((fraction none))
     ((fraction given)
      (determiner ((pre-det ((type fraction)))
                   (pre-det {^2 fraction}))))))
  ;; CBC - Version 2.3: Added a topicalizer, e.g. "How about that weather?"
  (alt topic (:index topic)
    (((topic none))
     ((topic given)
      (determiner ((pre-det ((type topic))))))))
                   ;(pre-det {^2 topic}))))))
  (alt multiplier (:index multiplier)
    (((multiplier none))
     ((multiplier given)
      (determiner ((pre-det ((type multiplier)))
                   (pre-det {^2 multiplier})))))))


(def-alt describer
  (:demo "Are there describers?")
  ;; Under NP (semantics x)
  ;; ***** "Soon to be released book" "man eating tiger"
  (((describer none))
   ((describer given)
    ({^ pattern} (dots describer dots head dots))
    (describer
     ((alt describer-cat (:index cat)
    (((cat adj))    ;; either a single adj
     ((cat ap)      ;; or an ap with no modifiers
      (describer none)
      (qualifier none))
     ((cat ap)
      (qualifier given))  ;; "the safest of all investments"
     ((cat list))   ;; no check on lists
     ((cat date))   ;; no check on dates
     ((cat verb)
      (ending past-participle)
      (modifier-type objective))
     ((cat verb)
      (ending present-participle)
      (modifier-type subjective)))))))))

(def-alt classifier (:demo "Is there a classifier?")
  (((classifier none))
   ((classifier given)
    (classifier ((synt-funct classifier)))
    ({^ pattern} (dots classifier head dots))
    (classifier
     ((alt classifier-cat (:index cat)
         ;; CBC - Version 2.3: Added a number of categories
         (((cat np-head))
          ((cat adj))
          ((cat ap))
          ((cat trivial-proper))
          ((cat basic-proper))
          ((cat compound-proper))
          ((cat np))
          ((cat numeric-range))
          ((cat date))
          ((cat list))
          ;; JR+9/1/92
          ((cat score))
          ((cat #(under list)))  ;; no check on lists
          ((cat #(under verb))
           (ending present-participle)
           (modifier-type subjective))))))
    (opt ((determiner ((gap yes))))))))


(def-alt qualifier
  (:demo "Is there a qualifier? Is it a PP, NP or a clause?")
  (((qualifier none))
   ((qualifier given)

    ;; dots added 4/24/93 to allow split quantity constituent,
    ;; e.g., 2 games in a row at home
    ({^ pattern} (dots head dots qualifier))

    (qualifier
     ((synt-funct qualifier)
      (alt qualifier-cat (:index cat)
    ;; CBC - Version 2.3: NP is rare, but PennTreeBank lists the following:
    ;;       (NP (NP $1.43) (NP a share))  (NP (NP five days) (NP a week))
    (((cat np) (restrictive yes))
     ((cat pp)
      (restrictive yes)
      ;; send features of qualifier just to np of pp.
      ;; This is messy - should be a different constituent.
      ;; default prep is "of".
      (opt ((prep ((lex "of")))))
      (np ((syntax {^2 syntax})
           (semantics {^2 semantics})
           (lex {^2 lex}))))
     ;; CBC - Version 2.3: First attempt at appositives:
     ;;                    "in particular", "e.g.", "such as"
     ((cat pp)
      (restrictive no)
      (opt ((prep ((lex "namely")
                   (punctuation ((before ",")
                                 (after ",")))))))
            (np ((syntax {^2 syntax})
                 (semantics {^2 semantics})
                 (lex {^2 lex}))))
     ((cat #(under list)))    ;; an heterogeneous list of qualifiers
     ;; "The elephant that came to brunch"
     ;; "The game he played so wonderfully"

     ;; "The fare specified on the ticket"
     ;; Debatable whether these are adjectives or
     ;; passive-participles - tests in general are:
     ;; If can add very - is an adjective
     ;; If can change "is" by "seems" or "remains" - is an adj
     ;; In general, want the ap to have a complement except if
     ;; there is a reason why not.
     ((cat #(under ap)))

     ;; CBC - Version 2.3: Adverbial qualifiers, such as "six years ago" "times past"
     ((cat #(under adv)))

     ((cat #(under clause))
      (alt clausal-qualifier (:index mood)
        (((mood relative))

         ;; The car repaired by the mechanic
         ((mood past-participle)
          (subject none)
          (binder none))

         ;; "The time remaining before the buzzer"
         ((mood present-participle)
          (subject none)
          (binder none))

         ;; "The game to be played tomorrow"
         ((mood to-infinitive)
          (binder none))

         ;; CBC - Version 2.3: A new qualifier type: "Their luck when it comes to money"
         ((mood bound-adverbial)
          (binder given))

         ((cat #(under clause))
          (mood for-to-infinitive)
          (binder none)))))))

      (alt qualifier-restrictive (:index restrictive)
       (((restrictive yes)
         (punctuation ((before none))))
        ;; CBC - Version 2.3: Allow specialized punctuation
        ((restrictive yes)
         (punctuation given))
        ((restrictive no)
         (punctuation given))
        ((restrictive no)
         (punctuation ((before ",") (after ",")))))))))))


(def-alt np-case (:index (syntax case))
  (:demo "Is this a possessive NP?")
  (((syntax ((case subjective))))
   ((syntax ((case given)))
    (syntax ((case ((alt (objective reflexive)))))))
   ((syntax ((case #(under possessive))))
    ;; possessive is an  feature.
    ;; YD: CHANGE possessive to be a feature of NP and not of HEAD
    (feature possessive))))





;; ============================================================
(provide "np")
;; ============================================================
