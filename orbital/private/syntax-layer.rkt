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

(define-syntax-parser fragment
  [(_ name:id
      (atoms [a-sym:id a-type:id . a-rest:expr] ...)
      (bonds [b-id1:id b-id2:id b-order:number . b-rest:expr] ...)
      (~optional (~seq #:exposed exposed-atom:id) #:defaults ([exposed-atom #'#f])))
   #'(let* ([m (mol (make-immutable-hash) empty)]
            [m (for/fold ([m m])
                         ([a-stx (list (atom-spec a-type . a-rest) ...)]
                          [a-key (list 'a-sym ...)])
                 (set-atom m a-key a-stx))]
            [m (for/fold ([m m])
                         ([b-stx (list (bond-spec b-id1 b-id2 b-order . b-rest) ...)])
                 (add-bond m b-stx))])
       (list m (if (equal? (syntax-e #'exposed-atom) #f) '() (list 'exposed-atom))))])

(define-syntax-parser combine-fragments
  [(_ name:id
      [frag1:id #:at atom1:id]
      [frag2:id #:at atom2:id]
      #:bt [bond-atom1:id bond-atom2:id order:number])
   #'(let* ([f1 frag1]
            [f2 frag2]
            [m1 (first f1)]
            [m2 (first f2)]
            [m (mol (make-immutable-hash) empty)]
            [m (for/fold ([m m])
                         ([atom (hash->list (mol-atoms m1))])
                 (set-atom m (car atom) (cdr atom)))]
            [m (let ([hydroxyl-O (hash-ref (mol-atoms m2) 'O1)]
                     [hydroxyl-H (hash-ref (mol-atoms m2) 'H1)])
                 (let ([m (set-atom m 'O11 hydroxyl-O)])
                   (set-atom m 'H12 hydroxyl-H)))]
            [m (for/fold ([m m])
                         ([b (mol-bonds m1)])
                 (add-bond m b))]
            [m (add-bond m (bond 'O11 'H12 1 #f))]
            [m (add-bond m (bond 'C1 'O11 1 #f))])
       m)])

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
                                    (list (bond 'C1 'H1 1 #f))))

  (define methyl
    (fragment methyl
              (atoms [C1 C] [H1 H] [H2 H] [H3 H])
              (bonds [C1 H1 1] [C1 H2 1] [C1 H3 1])
              #:exposed C1))

  (define hydroxyl
    (fragment hydroxyl
              (atoms [O1 O] [H1 H])
              (bonds [O1 H1 1])
              #:exposed O1))

  (define methanol
    (combine-fragments methanol
                       [methyl #:at C1]
                       [hydroxyl #:at O1]
                       #:bt [C1 O1 1]))

  (define (bond-set-equal? b1 b2)
    (and (equal? (bond-a1 b1) (bond-a1 b2))
         (equal? (bond-a2 b1) (bond-a2 b2))
         (equal? (bond-order b1) (bond-order b2))
         (equal? (bond-stereo b1) (bond-stereo b2))))

  (define (mol-equal? m1 m2)
    (and (equal? (mol-atoms m1) (mol-atoms m2))
         (equal? (length (mol-bonds m1)) (length (mol-bonds m2)))
         (andmap (λ (b1) (ormap (λ (b2) (bond-set-equal? b1 b2)) (mol-bonds m2)))
                 (mol-bonds m1))))

  (check-true (mol-equal? methanol
                           (mol (hash 'C1 (atom 'C #f #f #f)
                                     'H1 (atom 'H #f #f #f)
                                     'H2 (atom 'H #f #f #f)
                                     'H3 (atom 'H #f #f #f)
                                     'O11 (atom 'O #f #f #f)
                                     'H12 (atom 'H #f #f #f))
                               (list (bond 'C1 'H1 1 #f)
                                     (bond 'C1 'H2 1 #f)
                                     (bond 'C1 'H3 1 #f)
                                     (bond 'C1 'O11 1 #f)
                                     (bond 'O11 'H12 1 #f))))))
