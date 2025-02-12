#lang racket
(require pict3d
         racket/draw
         "explore.rkt")

(provide atom lone-pair bond bond-to sp sp2 sp3 HYDROGEN CARBON vw->radius)

(define (vw->radius vw)
  (/ vw 170))

(struct atom [size color])
(define HYDROGEN (atom 120 (make-color 255 255 255)))
(define CARBON (atom 170 (make-color 144 144 144)))

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

(define (bond-to atom [scale 1] [rotate? #f])
  (glue
   (bond scale rotate?)
   '(temp)
   ((orbitalize (basis 'temp (point-at origin +z))) atom)
   '(temp)))

(define (map-transforms transforms)
  
  (define tags (build-list (sub1 (length transforms)) (λ (n) (string->symbol (string-append "d" (number->string (add1 n)))))))
  (values
   tags
   (car transforms)
   (call-with-values
    (thunk
     (map (λ (tag transform) (basis tag transform)) tags (cdr transforms)))
    combine)))


(define (hybridize atom
                   #:groups groups
                   #:transforms transforms
                   #:R [R #f]
                   #:RBD [RBD 1]
                   #:rotate? [rotate? #f])
  (define (dirn dx dy dz) (dir-normalize (dir dx dy dz)))
  (define-values (tags origin-transform bases) (map-transforms transforms))
  (define GROUP
    (set-origin
     (foldr (λ (tag group acc) (glue acc (list tag) group))
            ((orbitalize bases) atom)
            tags
            groups)
     origin-transform))
  (cond [R (glue R '() GROUP)]
        [RBD (glue  (bond RBD rotate?) '(temp) GROUP '())]))

(define (sp [atom CARBON] #:d1 [d1 (bond-to HYDROGEN)] #:R [R #f] #:RBD [RBD 3])
  (hybridize
   atom
   #:groups (list d1)
   #:transforms (list (point-at origin +z) (point-at origin -z))
   #:R R
   #:RBD RBD
   #:rotate? (equal? RBD 2)))

(define (sp2 [atom CARBON] #:d1 [d1 (bond-to HYDROGEN)] #:d2 [d2 (bond-to HYDROGEN)] #:up [up (group empty-pict3d 'R)]#:R [R #f] #:RBD [RBD 2])
  (hybridize
   atom
   #:groups (list d1 d2 up)
   #:transforms (list (point-at origin +x) (point-at origin (angles->dir 120 0)) (point-at origin (angles->dir -120 0)) (point-at origin +z))
   #:R R
   #:RBD RBD))

(define (sp3 [atom CARBON] #:d1 [d1 (bond-to HYDROGEN)] #:d2 [d2 (bond-to HYDROGEN)] #:d3 [d3 (bond-to HYDROGEN)] #:R [R #f])
  (define (dirn dx dy dz) (dir-normalize (dir dx dy dz)))
  (hybridize
   atom
   #:groups (list d1 d2 d3)
   #:transforms (list
                     (point-at origin (dirn 1 1 1))    
                     (point-at origin (dirn 1 -1 -1))  
                     (point-at origin (dirn -1 -1 1))
                     (point-at origin (dirn -1 1 -1)))
   #:R R))

  













