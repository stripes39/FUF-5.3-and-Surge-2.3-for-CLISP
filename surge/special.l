;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:  -*-
;;; -----------------------------------------------------------------------
;;; File:         special.l
;;; Description:  Specialized grammatical constructs (e.g., address, date)
;;; Author:       Jacques Robin
;;; Created:      18 Jan 1993
;;; Modified:      5 Jul 1995 SURGE 2.2 VERSION
;;;                5 Nov 1995 Moved person-name here
;;;               12 Oct 1997 Date bug fix from Victor Essers (VE)
;;;               25 May 2005 SURGE 2.3 VERSION
;;;                           - Added institution as a semantic type
;;;                             (hospitals, universities, etc.)
;;;                           - Added a large number of date variations
;;;                           - Added am/pm absolute times for PTB
;;; -----------------------------------------------------------------------

;(in-package :FUG5)

(def-conj address
  (cat #(under address))
  (pattern (num side st-name st-type bldg-name bldg-type apt apt-num hood quadrant
            po-box po-box-num city state zip country))

  ;; Constraints on minimum co-occurrences
  ;; num ==> st-name
  ;; st-name ==> st-type
  ;; side & quadrant mutually exclusive
  ;; hood & quadrant mutually exclusive
  ;; zip & po-box mutually exclusive
  ;; (zip or po-box) ==> city & (state and/or country)

  (alt st-name
      (((num none) (st-name none) (st-type none) (cset ((- num st-name st-type))))
       ((st-name given)
    (alt (((st-name ((lex given) (cat phrase))))
          ((st-name ((value given) (cat ordinal))))
          ((st-name ((cat person-name))))
          ((st-name ((cat date))))))
    (st-type ((lex given) (cat phrase)))
    (opt ((num ((value given) (cat cardinal))))))))
  (alt hood-side-quadrant
      (((hood none)
    (quadrant none)
    (cset ((- hood quadrant)))
    (opt ((side ((lex given) (cat phrase))))))
       ((hood ((lex given) (cat phrase)))
    (alt (((apt-num none) (st-name none))
          ((apt-num given) (hood ((punctuation ((before ","))))))
          ((apt-num none) (st-name given) (hood ((punctuation ((before ","))))))))
    (quadrant none)
    (cset ((- quadrant)))
    (opt ((side ((lex given) (cat phrase))))))
       ((quadrant ((lex given) (cat phrase)))
    (alt (((apt-num none) (st-name none))
          ((apt-num given) (quadrant ((punctuation ((before ","))))))
          ((apt-num none)
           (st-name given)
           (quadrant ((punctuation ((before ","))))))))
    (hood none)
    (side none)
    (cset ((- hood side))))))
  (opt bldg ((bldg-name ((lex given) (cat phrase)))
         (alt bldg-type (((bldg-type ((lex given) (cat phrase))))
                 ((bldg-type ((lex "Bldg") (cat phrase)))
                 ((bldg-type ((lex "Building") (cat phrase)))))))))
  (opt ((apt-num ((lex given) (cat phrase)))
    (apt ((alt apt (((cat phrase) (lex "#") (punctuation ((before ","))))
            ((cat phrase) (lex "apt.") (punctuation ((before ","))))
            ((cat phrase) (lex "room") (punctuation ((before ","))))))))))
  (alt zip-pobox
      (((zip none) (po-box-num none) (cset ((- zip po-box po-box-num))))
       ((zip ((value given) (cat cardinal) (punctuation ((before ",")))))
    (city given)
    (alt (((state given)) ((country given))))
    (po-box none)
    (po-box-num none)
    (cset ((- po-box-num po-box))))
       ((po-box-num ((value given) (cat cardinal)))
    (po-box ((cat phrase) (lex "P.O. Box") (punctuation ((before ",")))))
    (city given)
    (alt (((state given)) ((country given))))
    (zip none)
    (cset ((- zip))))))
  (opt ((city ((lex given) (cat phrase)))
    (alt city (((quadrant given))
           ((po-box-num given) (city ((punctuation ((before ","))))))
           ((hood given) (city ((punctuation ((before ","))))))
           ((apt-num given) (city ((punctuation ((before ","))))))
           ((st-name given) (city ((punctuation ((before ","))))))
           ((quadrant none)
            (po-box-num none)
            (hood none)
            (apt-num none)
            (st-name none))))))
  (opt ((state ((lex given) (cat phrase)))
    (alt (((city given) (state ((punctuation ((before ","))))))
          ((po-box-num given) (state ((punctuation ((before ","))))))
          ((hood given) (state ((punctuation ((before ","))))))
          ((apt-num given) (state ((punctuation ((before ","))))))
          ((st-name given) (state ((punctuation ((before ","))))))
          ((city none)
           (po-box-num none)
           (hood none)
           (apt-num none)
           (cset ((- city po-box-num hood apt-num apt po-box st-name
             st-type num)))
           (st-name none))))))
  (opt ((country ((lex given) (cat phrase)))
    (alt (((zip given))
          ((city given) (country ((punctuation ((before ","))))))
          ((state given) (country ((punctuation ((before ","))))))
          ((po-box-num given) (country ((punctuation ((before ","))))))
          ((hood given) (country ((punctuation ((before ","))))))
          ((apt-num given) (country ((punctuation ((before ","))))))
          ((st-name given) (country ((punctuation ((before ","))))))
          ((city none)
           (state none)
           (po-box-num none)
           (hood none)
           (apt-num none)
           (st-name none)
           (cset ((- city state po-box-num hood apt-num apt po-box st-name
             st-type num)))))))))



(def-conj date

  (cat #(under date))

  ;; Possible patterns
  ;; June
  ;; Friday
  ;; 1999
  ;; the 13th

  ;; June 13th                                  day-num month
  ;; the 13th of June                                "
  ;; 6 / 13                                          "
  ;; June 1999                                  year month
  ;; Friday night                               day-part day-name
  ;; Friday the 13th                            day-name day-num

  ;; June 13th 1999                             day-num month year
  ;; the 13th of June 1999                             "     "
  ;; 6 / 13 / 1999                                     "     "
  ;; Friday the 13th of June                    day-name day-num month
  ;; Friday June 13th                                   "       "
  ;; Friday the 13th at night                   day-part day-name day-num
  ;; the night of Friday the 13th                       "       "

  ;; the night of June 13th 1999                year month day-num day-part
  ;; June 13th 1999, at night                       "             "
  ;; the 13th of June 1999, at night                "             "
  ;; 6 / 13 / 1999, at night                        "             "
  ;; the night of Friday the 13th of June       month day-num day-name day-part
  ;; Friday June 13th, at night                      "                "

  ;; the night of Friday June 13th 1999         year month day-num day-name day-part
  ;; Friday June 13th 1999, at night
  ;; Friday the 13th of June 1999, at night
  ;; Friday 6 / 13 / 1999, at night

  ;; The following October 1st
  ;; Around October 1st
  ;; Around 1999

  ;; Constraints on co-occurrences and positions:
  ;; month links year to day-num
  ;; day-num links month to day-name and day-part

  (alt day-num
      (;; 1999 | Around 1999 | 1999, the best year
       ((day-num none) (month none) (day-name none) (day-part none)
        (year given) (year ((cat cardinal)))
        (alt (((modifier ((cat adv) (lex given))))
              ((modifier none))))
        (qualifier ((alt (given none))
                    (punctuation ((before ((alt (given ","))))))))
        (pattern (modifier year qualifier)))
       ((day-num none)
    (alt day-num-none
        (
         ;; June | 1999 | June 1999 | Around June 1999 | June 1999, the best month
         ((day-name none)
          (day-part none)
          (day-range1 none)
          (alt (((modifier ((cat adv) (lex given))))
                ((modifier none))))
          (qualifier ((alt (given none))))
          (pattern (modifier month year qualifier))
          (opt ((month ((lex given) (cat phrase)))))
          (opt ((year ((value given) (format none) (cat cardinal))))))

          ((day-range1 given)
           (pattern (dots modifier day-name month day-range1 range day-range2 year qualifier dots))
           (alt (((modifier ((cat phrase) (lex given))))
                 ((modifier ((cat adv) (lex given))))
                 ((modifier none))))
           (qualifier ((alt (given none))))
           (month ((cat phrase)))
           (day-range1 ((cat cardinal) (digit yes)))
           (day-range2 ((cat cardinal) (digit yes)))
           (range ((cat conj) (lex given)))
           (opt ((year ((value given)
                        (cat cardinal) (format none)
                        (punctuation ((before ",")))))))
           (month-before-day-num yes))

         ;; Friday | Friday night
         ((year none)
          (month none)
          (day-name ((lex given) (cat phrase)))
          (pattern (day-name day-part))
          (opt ((day-part ((lex given) (cat phrase)))))))))
       ((day-num ((value given)))
    (alt day-num-given
        (
         ((alt day-num-month-realz
          (
           ;; Friday the 13th, at night | The night of Friday the 13th
           ((month none)
            (day-num ((alt (((cat ordinal)) ((cat cardinal))))))
            (pattern (dots day-name the day-num dots))
            (the ((cat phrase) (lex "the"))))

           ;; 6 / 13 | 6 / 13 / 1999 | 6 / 13, at night | Friday 6 / 13
           ((pattern (modifier day-name month day-num year qualifier dots))
            (alt (((modifier ((cat phrase) (lex given))))
                  ((modifier ((cat adv) (lex given))))
                  ((modifier none))))
            (day-num ((value given)
                      (cat cardinal)
                      (digit yes)
                      (punctuation ((before "/")))))
            (month ((value given) (cat cardinal) (digit yes)))
            (qualifier ((alt (given none))))
            (opt ((year ((value given)
                         (cat cardinal) (format none)
                         (punctuation ((before "/"))))))))

           ((month ((lex given) (cat phrase)))
            (alt month-day-num-order
             (
              ;; June 13th | June 13th 1999 | June 13th, at night |
              ;; The night of June 13th | Friday June 13th etc ...
              ;; (VE), 10-10-97, commented out (month ...) below
              ;; (not necessary):
              (
               ;;(month ((lex given) (cat phrase)))
               (pattern (dots definite2 describer modifier day-name month day-num year qualifier dots))
               (alt (((modifier ((cat phrase) (lex given))))
                     ((modifier ((cat adv) (lex given))))
                     ((modifier none))))
               ;; Next is for "_the_ following Oct. 1st" as cat date has no determiner feature
               (opt ((definite given) (definite yes) (definite2 ((cat phrase) (lex "the")))))
               (describer ((alt (((cat ap) (lex given)) ((cat adj) (lex given)) ((cat phrase) (lex ""))))))
               (qualifier ((alt (given none))))
               (day-num ((alt (((cat ordinal) (digit yes))
                               ((cat cardinal) (digit yes))))))
               ;; (VE), 10-10-97, added this line
               (opt ((day-num ((value given)))
                     (year ((value given)
                            (cat cardinal) (format none)
                            (punctuation ((before ",")))))))
               (month-before-day-num yes))

              ;; the 13th of June  | the 13th of June 1999 |
              ;; the 13th of June at night etc ...
              ((pattern (day-name the day-num of month year qualifier dots))
               (day-num ((alt (((cat ordinal)) ((cat cardinal))))))
               (month-before-day-num no)
               (qualifier ((alt (given none))))
               (the ((cat phrase) (lex "the")))
               (of ((cat phrase) (lex "of")))))))))

          (alt day-part-realz
          (((day-part none))

           ;; midnight|night|nighttime
           ;((day-part ((lex given) (cat phrase) (lex ((alt ("midnight" "night" "nighttime"))))))
           ; (pattern (day-part dots)))

           ;; The night of |  The morning of etc ...
           ((pattern (the day-part of dots))
            (day-part ((lex given) (cat phrase)))
            (the ((cat phrase) (lex "the")))
            (of ((cat phrase) (lex "of"))))

           ;; at night | in the evening etc
           ((pattern (dots prep day-part))
            (day-part ((lex given) (cat phrase)))
            (alt prep-choice
              (((day-part ((lex ((alt ("night" "midnight" "nighttime"))))))
                (prep ((cat prep) (lex "at") (punctuation ((before ","))))))
               ((prep ((cat phrase)
                       (lex "in the")
                       (punctuation ((before ","))))))))))))))
    (opt ((year ((value given) (format none) (cat cardinal)))))
    (opt ((day-name ((lex given) (cat phrase)))))))))

(def-conj absolute-time
  (cat #(under absolute-time))
  (hour ((value given) (digit yes) (cat cardinal)))
  (alt (((period am)
         (period-name ((lex ((alt ("a.m." "AM"))))
                       (cat phrase))))
        ((period pm)
         (period-name ((lex ((alt ("p.m." "PM"))))
                       (cat phrase))))))
  (colon ((cat phrase) (lex ":")))
  (alt (((minute ((value given) (digit yes) (cat cardinal)))
         (pattern (hour colon minute period-name)))
        ((pattern (hour period-name))))))

;;Added by Charles
(def-conj institution-name
  (cat #(under institution-name))
  (institution ((cat noun)))
  (pattern (institution practice))
  (alt institution-name-synt-funct (((synt-funct #(under head)))
                                    ((synt-funct none))))
  (alt institution-name-practice
      (((practice given) (practice ((cat noun))))
       ((practice none)))))



(def-conj person-name
  (cat #(under person-name))
  (animate yes)
  (pattern (title first-name middle-name nickname last-name dynasty))
  (alt person-name-synt-funct (((synt-funct #(under head)))
                   ((synt-funct none))))
  (alt person-name-dynasty
      (((dynasty given)
    (dynasty ((feature {^2 feature})))
    (dynasty ((alt (((cat #(under ordinal)) (digit roman))
            ((father no) (cat phrase) (lex "Jr."))
            ((father yes) (cat phrase) (lex "Sr.")))))))
       ((dynasty none))))

  ;; Constraints on minimum cooccurrences
  ;; last-name
  ;; first-name
  ;; nickname
  ;; first-name last-name
  ;; nickname last-name
  ;; first-name nickname last-name
  ;; first-name middle-name last-name
  ;; first-name dynasty
  ;; first-name last-name dynasty
  ;; first-name middle-name last-name dynasty
  ;; title last-name
  ;; title first-name last-name
  ;; title first-name middle-name last-name
  ;; title first-name middle-name last-name dynasty
  (alt person-name-pattern
      (((nickname none)
    (middle-name none)
    (opt ((title ((alt (((lex given) (cat phrase))
                ((cat #(under list)))))))))
    (alt (((last-name ((lex given)
               (cat noun)
               (feature {^2 feature})))
           (opt ((first-name ((lex given) (cat noun))))))
          ((last-name ((gap yes)))
           (opt ((first-name ((lex given) (cat noun))))))
          ((first-name ((lex given)
                (cat noun)
                (feature {^2 feature})))))))
       ((nickname ((lex given) (cat noun)))
    (middle-name none)
    (dynasty none)
    (title none)
    (opt ((first-name ((lex given)
               (cat noun)))))
    (alt (((last-name ((lex given)
               (cat noun)
               (feature {^2 feature}))))
          ((last-name ((gap yes))))
          ((first-name none)
           (nickname ((feature {^2 feature})))))))
       ((middle-name ((lex given) (cat noun)))
    (first-name ((lex given) (cat noun)))
    (last-name ((lex given)
            (cat noun)
            (feature {^2 feature})))
    (nickname none)
    (opt ((title ((alt (((lex given) (cat phrase))
                ((cat #(under list))
                 (common ((cat phrase)))))))))))))
  )


;; ============================================================
(provide "special")
;; ============================================================
