#lang typed/racket
(require pict3d
         typed/racket/draw
         "explore.rkt"
         "utils.rkt")

(provide atom lone-pair bond bond-to sp sp2 sp3 HYDROGEN CARBON vw->radius)

(: vw->radius (-> Real Real))
(define (vw->radius vw)
  (/ vw 170))

(define-type ColorResolvable (U RGBA
                                Real
                                (Listof Real)
                                (Vectorof Real)
                                FlVector
                                String
                                (Instance Color%)))

(struct atom [(size : Real)
              (color : ColorResolvable)])
(define-type Atom atom)

(define HYDROGEN (atom 120 (make-color 255 255 255)))
(define CARBON (atom 170 (make-color 144 144 144)))

(: orbitalize (-> Pict3Ds (-> Atom Pict3D)))
(define ((orbitalize bases) atom)
  (combine
   (with-color (rgba (atom-color atom))
     (sphere
      origin
      (vw->radius (atom-size atom))))
   bases))

(define lone-pair (set-origin
                   (combine
                    (move-z (sphere origin .25) -.5)
                    (move-z (sphere origin .25) .5)

                    (with-color (rgba "Light Blue") (ellipsoid origin (dir 1 1 2) #:inside? #t)))
                   (point-at (pos 0 0 2) -z)))

(: bond (->* () (Real Boolean) Pict3D))
(define (bond [scale 1] [rotate? #f])
  (define BOND (cylinder origin (dir .125 .125 2)))
  (define MINI-BOND (cylinder origin (dir .09 .09 2)))
  (define BOND-BEND-RIGHT (bend (tessellate MINI-BOND) 90))
  (define BOND-BEND-LEFT  (bend (tessellate MINI-BOND) -90))
  (define PI-BOND-Y (combine (move-x BOND-BEND-RIGHT -1) (move-x BOND-BEND-LEFT 1)))
  (define PI-BOND-X (rotate-z PI-BOND-Y 90))
  (define t1 (point-at (pos 2 0 0) -x))
  (define t2 (point-at (pos -2 0 0) +x))
  (set-origin
   (combine
    (rotate-y (match scale
                (1 BOND)
                (2 (if rotate?  (combine BOND PI-BOND-Y) (combine BOND PI-BOND-X)))
                (3 (combine BOND PI-BOND-Y PI-BOND-X))) 90)

    (basis 'temp t1))
   t2))

(: bond-to (->* (Atom) (Real Boolean) Pict3D))
(define (bond-to atom [scale 1] [rotate? #f])
  (glue
   (bond scale rotate?)
   '(temp)
   ((orbitalize (basis 'temp (point-at origin +z))) atom)
   '(temp)))


(: map-transforms (-> (Listof Affine) (Values (Listof Symbol) Affine Pict3Ds)))
(define (map-transforms transforms)
  (define tags (build-list (sub1 (length transforms))
                           (λ ((n : Integer)) (sym+ "d" (add1 n)))))
  (values
   tags
   (car transforms)
   (call-with-values
    (thunk
     (map (λ ((tag : Symbol) (transform : Affine)) (basis tag transform)) tags (cdr transforms)))
    combine)))

(: hybridize (->* (Atom #:groups (Listof Pict3D) #:transforms (Listof Affine))
                  (#:R (U False Pict3D) #:RBD Real #:rotate? Boolean)
                  Pict3D))
(define (hybridize atom
                   #:groups groups
                   #:transforms transforms
                   #:R [R #f]
                   #:RBD [RBD 1]
                   #:rotate? [rotate? #f])
  (define-values (tags origin-transform bases) (map-transforms transforms))
  (define GROUP
    (set-origin
     (foldr (λ ((tag : Tag) (group : Pict3D) (acc : Pict3D)) (glue acc (list tag) group))
            ((orbitalize bases) atom)
            tags
            groups)
     origin-transform))
  (cond [R (glue R '() GROUP)]
        [RBD (glue  (bond RBD rotate?) '(temp) GROUP '())]))

(: sp (->* () (Atom #:d1 Pict3D #:R (U False Pict3D) #:RBD Real) Pict3D))
(define (sp [atom CARBON] #:d1 [d1 (bond-to HYDROGEN)] #:R [R #f] #:RBD [RBD 3])
  (hybridize
   atom
   #:groups (list d1)
   #:transforms (list (point-at origin +z) (point-at origin -z))
   #:R R
   #:RBD RBD
   #:rotate? (equal? RBD 2)))

(: sp2 (->* () (Atom #:d1 Pict3D #:d2 Pict3D #:up Pict3D #:R (U False Pict3D) #:RBD Real) Pict3D))
(define (sp2 [atom CARBON]
             #:d1 [d1 (bond-to HYDROGEN)]
             #:d2 [d2 (bond-to HYDROGEN)]
             #:up [up (group empty-pict3d 'R)]
             #:R [R #f]
             #:RBD [RBD 2])
  (hybridize
   atom
   #:groups (list d1 d2 up)
   #:transforms (list (point-at origin +x)
                      (point-at origin (angles->dir 120 0))
                      (point-at origin (angles->dir -120 0))
                      (point-at origin +z))
   #:R R
   #:RBD RBD))

(: sp3 (->* () (Atom #:d1 Pict3D #:d2 Pict3D #:d3 Pict3D #:R (U False Pict3D)) Pict3D))
(define (sp3 [atom CARBON]
             #:d1 [d1 (bond-to HYDROGEN)]
             #:d2 [d2 (bond-to HYDROGEN)]
             #:d3 [d3 (bond-to HYDROGEN)]
             #:R [R #f])
  (hybridize
   atom
   #:groups (list d1 d2 d3)
   #:transforms (list
                 (point-at origin (dirn 1 1 1))
                 (point-at origin (dirn 1 -1 -1))
                 (point-at origin (dirn -1 -1 1))
                 (point-at origin (dirn -1 1 -1)))
   #:R R))
