#lang racket

(provide (contract-out (mol3d->pict3d (-> mol3d? pict3d?))))

(require "types.rkt"
         "periodic-table.rkt"
         pict3d)

; scale applied to the vanderwaals radius of an atom
; to get to pict3d units
(define atomic-radius-scale (make-parameter .0025))

(define (atom3d->pict3d a)
  (match-define (atom3d id num x y z) a)
  (define elem (list-ref periodic-table (sub1 num)))
  (define rad (* (atomic-radius-scale)
                 (element-atomic-radius elem)))
  (combine
   (with-color
       (rgba (if (element-cpk-color elem)
                 (element-cpk-color elem)
                 (rgba "black")))
     (sphere (pos x y z) rad))
   (basis id (move (dir x y z)))))

(define (atoms3d->pict3d as)
  (apply combine (map atom3d->pict3d as)))

; radius of a bond in pict3d units
(define bond-radius (make-parameter .08))

; draws a cylinder between two points with radius r
(define (bond-pict p1 p2 [order 1] [rad (bond-radius)])
  (define l/2 (/ (pos-dist p1 p2) 2))
  (define cyl (move-z (cylinder origin (dir rad rad l/2)) l/2))
  (define d (* rad 1.5)) ; bigger than radius
  (transform
   (cond [(equal? order 1) cyl]
         [(equal? order 2) (combine (move-x cyl d)
                                    (move-x cyl (- d)))]
         [(equal? order 3) (combine (move-x cyl d)
                                    cyl
                                    (move-x cyl (- d)))]
         [else (error "higher order bonds not supported: " order)])
   (point-at p1 p2)))

(define (add-bond p b)
  (match-define (bond3d a1 a2 order) b) ; TODO handle order correctly
  (define p1 (affine-origin (find-group-transform p (list a1))))
  (define p2 (affine-origin (find-group-transform p (list a2))))
  (combine p (bond-pict p1 p2 order)))

(define (add-bonds p bs)
  (foldl (λ (b acc) (add-bond acc b)) p bs))

(define (mol3d->pict3d m)
  (match-define (mol3d atoms bonds) m)
  (define p (atoms3d->pict3d atoms))
  (add-bonds p bonds))
