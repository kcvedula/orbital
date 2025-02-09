#lang racket
(require "functional-groups.rkt")
(require "explore.rkt")
(require "orbital1.rkt")
(require pict3d)


; TODO - Realistic oxygens in sp2, mechanism for carbon chain bonded to something
(define -NONE (group empty-pict3d 'R))
(define -OH_sp2 (sp2 OXYGEN #:d1 lone-pair #:up lone-pair #:RBD 1))


(define mol (sp2 #:d1 -Et #:R (sp2 #:d2 -Et)))

(define (alkene #:da1 [a1 -H] #:da2 [a2 -H] #:db1 [b1 -H] #:db2 [b2 -H])
  (sp2 #:d1 a1 #:d2 b1 #:R (sp2 #:d1 a2 #:d2 b2)))

(define (spin sp3-atom [count 1])
  (replace-group sp3-atom '(R) (λ (p) (rotate-z p (* count 30 )))))
(define temp (sp3 #:d1 (bond-to OXYGEN)))
(explore temp)
(explore (spin temp))
(explore (spin (spin temp)))





