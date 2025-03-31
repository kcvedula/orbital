#lang typed/racket
(require "functional-groups.rkt"
         "explore.rkt"
         "orbital1.rkt"
         pict3d)

; TODO - Realistic oxygens in sp2, mechanism for carbon chain bonded to something
(define -NONE (group empty-pict3d 'R))
(define -OH_sp2 (sp2 OXYGEN #:d1 lone-pair #:up lone-pair #:RBD 1))

(: alkene (->* () (#:da1 Pict3D #:da2 Pict3D #:db1 Pict3D #:db2 Pict3D) Pict3D))
(define (alkene #:da1 [a1 -H] #:da2 [a2 -H] #:db1 [b1 -H] #:db2 [b2 -H])
  (sp2 #:d1 a1 #:d2 b1 #:R (sp2 #:d1 a2 #:d2 b2)))

(: spin (-> Pict3D Real Pict3D))
(define (spin sp3-atom [angle 0])
  (rotate-z sp3-atom angle))

(: newman-projection (->* () (#:b1 Pict3D #:b2 Pict3D #:b3 Pict3D
                              #:f1 (U False Pict3D) #:f2 Pict3D #:f3 Pict3D
                              #:spin Real)
                          Pict3D))
(define (newman-projection
         #:b1 [b1 -H] #:b2 [b2 -H] #:b3 [b3 -H]
         #:f1 [f1 #f] #:f2 [f2 -H] #:f3 [f3 -H]
         #:spin [spin-num 0])
  (sp3 #:d1 (spin (sp3 #:d1 b1 #:d3 b3 #:d2 b2) (+ spin-num 60)) #:d2 f2 #:d3 f3 #:R f1))

(: chair/axial-root (->* ()
                         (#:a1 (U False Pict3D) #:e1 Pict3D
                          #:a2 Pict3D #:e2 Pict3D
                          #:a3 Pict3D #:e3 Pict3D
                          #:a4 Pict3D #:e4 Pict3D
                          #:a5 Pict3D #:e5 Pict3D
                          #:a6 Pict3D #:e6 Pict3D)
                         Pict3D))
(define (chair/axial-root #:a1 [a1 #f] #:e1 [e1 -H]
                          #:a2 [a2 -H] #:e2 [e2 -H]
                          #:a3 [a3 -H] #:e3 [e3 -H]
                          #:a4 [a4 -H] #:e4 [e4 -H]
                          #:a5 [a5 -H] #:e5 [e5 -H]
                          #:a6 [a6 -H] #:e6 [e6 -H])
  (sp3 #:d1 e1 #:R a1
       #:d2 (spin (sp3 #:d1 e2 #:d3 a2
                       #:d2 (spin (sp3 #:d1 a3 #:d3 e3
                                       #:d2 (spin (sp3 #:d2 a4 #:d3 e4
                                                       #:d1 (sp3
                                                             #:d1 a5 #:d3 e5
                                                             #:d2 (spin
                                                                   (sp3 #:d2 a6 #:d3 e6) 60)))
                                                  60)) 60)) 60)))

(: chair/equatorial-root (->* ()
                              (#:a1 Pict3D #:e1 (U False Pict3D)
                               #:a2 Pict3D #:e2 Pict3D
                               #:a3 Pict3D #:e3 Pict3D
                               #:a4 Pict3D #:e4 Pict3D
                               #:a5 Pict3D #:e5 Pict3D
                               #:a6 Pict3D #:e6 Pict3D)
                              Pict3D))
(define (chair/equatorial-root #:a1 [a1 -H] #:e1 [e1 #f]
                               #:a2 [a2 -H] #:e2 [e2 -H]
                               #:a3 [a3 -H] #:e3 [e3 -H]
                               #:a4 [a4 -H] #:e4 [e4 -H]
                               #:a5 [a5 -H] #:e5 [e5 -H]
                               #:a6 [a6 -H] #:e6 [e6 -H])
  (sp3 #:d3 a1 #:R e1 #:d1
       (sp3 #:d1 e2 #:d2 a2
            #:d3 (sp3 #:d1 e3 #:d3 a3
                      #:d2 (spin (sp3 #:d1 a4 #:d3 e4 #:d2
                                      (spin (sp3 #:d2 a5 #:d3 e5 #:d1
                                                 (sp3 #:d1 a6 #:d3 e6)) 60)) 60)))))

(define (test-chair/axial-root)
  (explore (chair/axial-root
            #:a1 #f #:e1 -Br
            #:a2 -Cl #:e2 -Br
            #:a3 -Cl #:e3 -Br
            #:a4 -Cl #:e4 -Br
            #:a5 -Cl #:e5 -Br
            #:a6 -Cl #:e6 -Br)))

(define (test-chair/equatorial-root)
  (explore (chair/equatorial-root
            #:a1 -Cl #:e1 #f
            #:a2 -Cl #:e2 -Br
            #:a3 -Cl #:e3 -Br
            #:a4 -Cl #:e4 -Br
            #:a5 -Cl #:e5 -Br
            #:a6 -Cl #:e6 -Br)))

(explore (chair/equatorial-root #:e4 -tBu #:e1 (spin -tBu 60)))
