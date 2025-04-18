#lang racket

(provide
 (contract-out
  (mol->cml (-> mol? cml?))))

(require xml
         (except-in
          "types.rkt"
          struct:element
          element-name
          element?
          element))

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

