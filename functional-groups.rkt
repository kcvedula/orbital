#lang racket

(require "orbital1.rkt")
(require "explore.rkt")
(require racket/draw)
(require pict3d)

(provide (all-defined-out))
; Atoms, already get HYDROGEN AND CARBON
(define HELIUM
  (atom 140 (make-color 217 255 255)))
(define NITROGEN
  (atom 155 (make-color 48 80 248)))
(define OXYGEN
  (atom 152 (make-color 255 13 13)))
(define FLUORINE
  (atom 135 (make-color 144 224 80)))
(define SILICON
  (atom 210 (make-color 240 200 160)))
(define PHOSPHOROUS
  (atom 180 (make-color 255 128 0)))
(define SULFUR
  (atom 180 (make-color 255 255 48)))
(define CHLORINE
  (atom 175 (make-color 31 240 31)))
(define BROMINE
  (atom 183 (make-color 166 41 41)))
(define IODINE
  (atom 198  (make-color 148 0 148)))
  
; 1 valence 
(define -H (bond-to HYDROGEN))
(define -Cl (bond-to CHLORINE))
(define -Br (bond-to BROMINE))
(define -Iodine (bond-to IODINE))

; Carbon Groups

(define (carbon-chain [n 1] [R #f] #:hetero [hetero -H])
  (cond
    [(equal? n 1) (sp3 #:d1 hetero)]
    [else  (sp3 #:R R #:d3 (carbon-chain (sub1 n) #f #:hetero hetero))]
    ))

; normal
(define -Me (carbon-chain 1))
(define -Et (carbon-chain 2))
(define -Pr (carbon-chain 3))
(define -Bu (carbon-chain 4))
(define -Pe (carbon-chain 5))
(define -Hx (carbon-chain 6))

; iso
(define -iPr (sp3 #:d2 -Me #:d3 -Me))
(define -iBu (sp3 #:d2 -iPr))
(define -iPe (sp3 #:d2 -iBu))

; sec
(define -secBu (sp3 #:d1 -Me #:d2 -Et #:d3 -H))

; tert
(define -tBu (sp3 #:d1 -Me #:d2 -Me #:d3 -Me))
(define -tPe (sp3 #:d1 -Et #:d2 -Me #:d3 -Me))
(define -tHx (sp3 #:d1 -Et #:d2 -Me #:d3 -Me))

; linear functional groups

(define -OH (sp3 OXYGEN #:d1 -H #:d2 lone-pair #:d3 lone-pair))
(define -NH3 (sp3 NITROGEN #:d3 lone-pair))

(define -NITRILE (sp #:d1 (sp NITROGEN #:d1 lone-pair) #:RBD 1))

(define -VINYL (sp #:RBD 2 #:d1 (sp2 NITROGEN)))

(define (carbonyl d1 [R #f])
  (sp2 CARBON #:d1 d1 #:d2 (sp2 OXYGEN #:d1 lone-pair #:d2 lone-pair)  #:R R #:RBD 1))

(define (aldehyde [R #f]) (carbonyl -H R))

(define (ether d1 [R #f])
  (sp3 OXYGEN #:d1 d1 #:d2 lone-pair #:d3 lone-pair #:R R))

; cyclic functional groups

(define (aryl #:d1 [d1 -H] #:d2 [d2 -H] #:d3 [d3 -H] #:d4 [d4 -H] #:d5 [d5 -H] #:R [R #f])
  (sp2 #:R R
       #:d1 (group empty-pict3d 'R)
       #:RBD 1
       #:d2 (sp2 #:RBD 2
                 #:d1 d1
                 #:d2 (sp2 #:RBD 1
                           #:d2 d2
                           #:d1 (sp2 #:RBD 2
                                     #:d1 d3
                                     #:d2 (sp2 #:RBD 1
                                               #:d2 d4
                                               #:d1 (sp2 #:d1 d5 #:RBD 2)))))))
(define -Ph (aryl))

