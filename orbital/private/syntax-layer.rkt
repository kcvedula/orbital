; TODO: consider using syntax-parse
#lang racket

(require "core.rkt"
         syntax/parse
         syntax/parse/define)

(define-syntax-parser atom-spec
  [(_ sym:id
      (~optional (~seq #:mass mass:number) #:defaults ([mass #'#f]))
      (~optional (~seq #:chirality chiral:id) #:defaults ([chiral (quote-syntax #f)]))
      (~optional (~seq #:charge charge:number) #:defaults ([charge #'#f])))
   #:with chiral-symbol (if (equal? (syntax-e #'chiral) #f) #'#f #''chiral)
   #'(atom 'sym mass chiral-symbol charge)])

(define-syntax-parser bond-spec
  [(_ id1:id id2:id order:number
      (~optional (~seq #:stereo stereo:id) #:defaults ([stereo #'#f])))
   #'(bond 'id1 'id2 order stereo)])

(define-syntax-parser molecule
  [(_ name:id
      (atoms [a-sym:id a-type:id . a-rest:expr] ...)
      (bonds [b-id1:id b-id2:id b-order:number . b-rest:expr] ...))
   #'(let* ([m (mol (make-immutable-hash) empty)]
            [m (for/fold ([m m])
                         ([a-stx (list (atom-spec a-type . a-rest) ...)]
                          [a-key (list 'a-sym ...)])
                 (set-atom m a-key a-stx))]
            [m (for/fold ([m m])
                         ([b-stx (list (bond-spec b-id1 b-id2 b-order . b-rest) ...)])
                 (add-bond m b-stx))])
       m)])

(module+ test
  (require rackunit)

  (define water
    (molecule water
              (atoms [O O] [H1 H] [H2 H])
              (bonds [O H1 1] [O H2 1])))

  (check-equal? water (mol (hash 'H1 (atom 'H #f #f #f)
                                 'H2 (atom 'H #f #f #f)
                                 'O (atom 'O #f #f #f))
                           (list (bond 'O 'H2 1 #f) (bond 'O 'H1 1 #f))))


  (define labeled-carbon
    (molecule labeled-carbon
              (atoms
               [C1 C #:mass 13 #:chirality R #:charge 0]
               [H1 H])
              (bonds
               [C1 H1 1])))

  (check-equal? labeled-carbon (mol (hash 'C1 (atom 'C 13 'R 0)
                                          'H1 (atom 'H #f #f #f))
                                    (list (bond 'C1 'H1 1 #f)))))
