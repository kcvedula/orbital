#lang racket

(provide
 (contract-out
  (empty-mol mol?)
  (set-atom (-> mol? positive-integer? atom? mol?))
  (add-bond (-> mol? bond? mol?))
  (mol->cml (-> mol? cml?))))

(require xml
         (except-in
          "types.rkt"
          struct:element
          element-name
          element?
          element))

(define empty-mol (mol (make-immutable-hash) empty))

(define (set-atom m id a)
  (match-define (mol atoms bonds) m)
  (define atoms2 (hash-set atoms id a))
  (mol atoms2 bonds))

; doesn't check if a bond between two atoms already exists
(define (add-bond m b)
  (match-define (mol atoms bonds) m)
  (mol atoms (cons b bonds)))

(define (atom->xml id a)
  `(atom ((id ,(number->string id))
          (elementType ,(symbol->string (an-element-symbol-v (atom-element a))))
          ,@(if (atom-mass-number a)
                `((isotope ,(number->string (atom-mass-number a))))
                '())
          ,@(if (atom-formal-charge a)
                `((formalCharge ,(number->string (atom-formal-charge a))))
                '()))))

(define (bond->xml b)
  `(bond ((atomRefs2 ,(string-append
                        (number->string (bond-id1 b)) " "
                        (number->string (bond-id2 b))))
          (order ,(number->string (bond-order b)))
          ,@(if (bond-stereo b)
                `((stereo ,(symbol->string (bond-stereo b))))
                '()))))

(define (mol->xexpr m)
    `(molecule
       ()
       (atomArray
        ()
        ,@(for/list ([(id a) (in-hash (mol-atoms m))])
            (atom->xml id a)))
       (bondArray ()
                  ,@(map bond->xml (mol-bonds m)))))

(define (mol->cml m)
  (cml (xexpr->string (mol->xexpr m))))

