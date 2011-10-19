(make-package :FUG5)
(in-package :FUG5)

(load "fuf53-load.lisp")

(load-fug5-slow)

(define-procedural-type 'cset #'unify-cset
      :syntax #'check-cset
      :relocater #'relocate-pattern)

(load "surge/gr.l")

(grammar-p)
