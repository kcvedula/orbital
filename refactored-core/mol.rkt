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
  `(atom ((id ,(symbol->string id))
          (elementType ,(symbol->string (atom-element a)))
          ,@(if (atom-mass-number a)
                `((isotope ,(number->string (atom-mass-number a))))
                '())
          ,@(if (atom-formal-charge a)
                `((formalCharge ,(number->string (atom-formal-charge a))))
                '()))))

(define (bond->xml b)
  `(bond ((atomRefs2 ,(string-append
                        (symbol->string (bond-a1 b)) " "
                        (symbol->string (bond-a2 b))))
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

; CH4 example
(define H (atom 'H #f #f #f))
(define C (atom 'C #f #f #f))

(define CH4
  (let* ((m empty-mol)
         (m (set-atom m 'a1 C))
         (m (set-atom m 'a2 H))
         (m (set-atom m 'a3 H))
         (m (set-atom m 'a4 H))
         (m (set-atom m 'a5 H))
         (m (add-bond m (bond 'a1 'a2 1 #f)))
         (m (add-bond m (bond 'a1 'a3 1 #f)))
         (m (add-bond m (bond 'a1 'a4 1 #f)))
         (m (add-bond m (bond 'a1 'a5 1 #f))))
  m))
