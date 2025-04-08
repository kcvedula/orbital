; TODO: consider using syntax-parse
#lang racket

(require "core.rkt"
         syntax/parse
         syntax/parse/define)

(provide fragment molecule make-chain ring-molecule combine-fragments combine-into-fragment)

;; =-=-=-=- molecule grammar =-=-=-=-
;;   molecule = (molecule name-id
;;                (atoms [atom-id atom-type-id atom-option ...] ...)
;;                (bonds [atom1-id atom2-id order-num bond-option ...] ...))
;;   atom-option = #:mass number
;;               | #:chirality chirality-id
;;               | #:charge number
;;   bond-option = #:stereo stereo-id


(module+ test
  #;(molecule methane
              (atoms [C C] [H1 H] [H2 H] [H3 H] [H4 H])
              (bonds [C H1 1] [C H2 1] [C H3 1] [C H4 1]))

  ;; ==>

  #;(let* ([m (mol (make-immutable-hash) empty)]
           [m (for/fold ([m m])
                        ([a-stx (list (atom 'C #f #f #f)
                                      (atom 'H #f #f #f)
                                      (atom 'H #f #f #f)
                                      (atom 'H #f #f #f)
                                      (atom 'H #f #f #f))]
                         [a-key (list 'C 'H1 'H2 'H3 'H4)])
                (set-atom m a-key a-stx))]
           [m (for/fold ([m m])
                        ([b-stx (list (bond 'C 'H1 1 #f)
                                      (bond 'C 'H2 1 #f)
                                      (bond 'C 'H3 1 #f)
                                      (bond 'C 'H4 1 #f))])
                (add-bond m b-stx))])
      m))

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
   ; TODO: factor out
   #'(let* ([m (mol (make-immutable-hash) empty)]
            [m (for/fold ([m m])
                         ([a-stx (list (atom-spec a-type . a-rest) ...)]
                          [a-key (list 'a-sym ...)])
                 (set-atom m a-key a-stx))]
            [m (for/fold ([m m])
                         ([b-stx (list (bond-spec b-id1 b-id2 b-order . b-rest) ...)])
                 (add-bond m b-stx))])
       m)])

;; =-=-=-=- fragment grammar =-=-=-=-
;; (fragment name
;;          (atoms [atom-symbol atom-type
;;                  (#:mass mass-number)?
;;                  (#:chirality chirality-id)?
;;                  (#:charge charge-number)?] ...)
;;          (bonds [atom1-id atom2-id bond-order
;;                  (#:stereo stereo-id)?] ...)
;;          (#:exposed exposed-atom-id)?)

(module+ test
  #;(fragment methyl-group
              (atoms [C1 C]
                     [H1 H]
                     [H2 H]
                     [H3 H])
              (bonds [C1 H1 1]
                     [C1 H2 1]
                     [C1 H3 1])
              #:exposed C1)
  ;; ===>
  #;(list
     (mol (hash 'C1 (atom 'C #f #f #f)
                'H1 (atom 'H #f #f #f)
                'H2 (atom 'H #f #f #f)
                'H3 (atom 'H #f #f #f))
          (list (bond 'C1 'H1 1 #f)
                (bond 'C1 'H2 1 #f)
                (bond 'C1 'H3 1 #f)))
     '(C1)))

;; TODO: use a dedicated struct as IR rather than list?
(define-syntax-parser fragment
  [(_ name:id
      (atoms [a-sym:id a-type:id . a-rest:expr] ...)
      (bonds [b-id1:id b-id2:id b-order:number . b-rest:expr] ...)
      (~optional (~seq #:exposed exposed-atom:id) #:defaults ([exposed-atom #'#f])))
   ; TODO: factor out
   #'(let* ([m (mol (make-immutable-hash) empty)]
            [m (for/fold ([m m])
                         ([a-stx (list (atom-spec a-type . a-rest) ...)]
                          [a-key (list 'a-sym ...)])
                 (set-atom m a-key a-stx))]
            [m (for/fold ([m m])
                         ([b-stx (list (bond-spec b-id1 b-id2 b-order . b-rest) ...)])
                 (add-bond m b-stx))])
       (list m (if (equal? (syntax-e #'exposed-atom) #f) '() (list 'exposed-atom))))])


; Generate a ring of atoms with specified size
(define-syntax-parser make-ring
  [(_ size:number atom-type:expr)
   #'(let* ([atom-ids (for/list ([i (in-range 1 (add1 size))])
                        (string->symbol (format "C~a" i)))]
            [bond-pairs (append
                         (for/list ([i (in-range size)])
                           (cons (list-ref atom-ids i)
                                 (list-ref atom-ids (modulo (add1 i) size))))
                         (list atom-ids))])
       bond-pairs)])

; Generate a linear chain of atoms
(define-syntax-parser make-chain
  [(_ length:number atom-type:expr)
   #'(let* ([atom-ids (for/list ([i (in-range 1 (add1 length))])
                        (string->symbol (format "C~a" i)))]
            [bond-pairs (append
                         (for/list ([i (in-range (sub1 length))])
                           (cons (list-ref atom-ids i)
                                 (list-ref atom-ids (add1 i))))
                         ; Return the atom IDs as well for reference
                         (list atom-ids))])
       bond-pairs)])

; Helper function to generate atom and bond lists for molecules
(define (build-molecule-components ring-bonds additional-atoms additional-bonds)
  (let* ([ring-atoms (last ring-bonds)]
         [all-atoms (append ring-atoms additional-atoms)]
         [atom-specs (for/list ([a all-atoms])
                       (list a (car (symbol->string a))))]
         [bond-specs (append
                      (for/list ([b (drop-right ring-bonds 1)])
                        (list (car b) (cdr b) 1))
                      additional-bonds)])
    (values atom-specs bond-specs)))

; Macro for quickly creating a ring-based molecule
(define-syntax-parser ring-molecule
  [(_ name:id
      ring-size:number
      ring-atom-type:id
      (~optional (~seq #:additional-atoms additional-atoms:expr) #:defaults ([additional-atoms #'null]))
      (~optional (~seq #:additional-bonds additional-bonds:expr) #:defaults ([additional-bonds #'null])))
   #'(let* ([ring-bonds (make-ring ring-size 'ring-atom-type)]
            [atom-bond-lists (build-molecule-components ring-bonds additional-atoms additional-bonds)])
       #`(molecule #,name
                   (atoms #,@(car atom-bond-lists))
                   (bonds #,@(cadr atom-bond-lists))))])

; Predefined common molecular fragments
#;(define methyl-group
  (fragment methyl-group
            (atoms [C C] [H1 H] [H2 H] [H3 H])
            (bonds [C H1 1] [C H2 1] [C H3 1])
            #:exposed C))

#;(define ethyl-group
  (fragment ethyl-group
            (atoms [C1 C] [C2 C] [H1 H] [H2 H] [H3 H] [H4 H] [H5 H])
            (bonds [C1 C2 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [C2 H4 1] [C2 H5 1])
            #:exposed C2))

#;(define hydroxyl-group
  (fragment hydroxyl-group
            (atoms [O O] [H H])
            (bonds [O H 1])
            #:exposed O))

#;(define carboxyl-group
  (fragment carboxyl-group
            (atoms [C C] [O1 O] [O2 O] [H H])
            (bonds [C O1 2] [C O2 1] [O2 H 1])
            #:exposed C))

#;(define amino-group
  (fragment amino-group
            (atoms [N N] [H1 H] [H2 H])
            (bonds [N H1 1] [N H2 1])
            #:exposed N))

(define-syntax-parser macrolide-ring
  [(_ name:id
      (~optional (~seq #:ring-size ring-size:number) #:defaults ([ring-size #'14]))
      (~optional (~seq #:o-positions o-positions:expr) #:defaults ([o-positions #'(1 3 5 6 9 11 13)])))
   #'(let* ([ring-bonds (make-ring ring-size 'C)]
            [ring-atoms (last ring-bonds)]
            [oxygen-atoms (for/list ([pos o-positions])
                            (string->symbol (format "O~a" pos)))]
            [additional-bonds (for/list ([o oxygen-atoms]
                                         [pos o-positions])
                                (list (list-ref ring-atoms (sub1 pos)) o 1))]
            [h-atoms (for/list ([i (in-range 1 (add1 (* 2 ring-size)))])
                       (string->symbol (format "H~a" i)))]
            [h-bonds '()]
            [atom-specs (append
                         (for/list ([a ring-atoms]) (list a 'C))
                         (for/list ([o oxygen-atoms]) (list o 'O))
                         (for/list ([h h-atoms]) (list h 'H)))]
            [bond-specs (append
                         (for/list ([b (drop-right ring-bonds 1)])
                           (list (car b) (cdr b) 1))
                         additional-bonds
                         h-bonds)])
       #`(fragment #,name
                   (atoms #,@atom-specs)
                   (bonds #,@bond-specs)
                   #:exposed #,(list-ref ring-atoms 2)))])

(define-syntax-parser combine-fragments
  [(_ name:id
      [frag1:id #:at atom1:id]
      [frag2:id #:at atom2:id]
      #:bt [bond-atom1:id bond-atom2:id order:number])
   #'(let* ([f1 frag1]
            [f2 frag2]
            [m1 (first f1)]
            [m2 (first f2)]
            [exposed1 (second f1)]
            [exposed2 (second f2)]
            [m (mol (make-immutable-hash) empty)]
            [m (for/fold ([m m])
                         ([atom (hash->list (mol-atoms m1))])
                 (set-atom m (car atom) (cdr atom)))]
            [atom-map (make-hash)]
            [m (for/fold ([m m])
                         ([atom (hash->list (mol-atoms m2))])
                 (let* ([orig-id (car atom)]
                        [atom-value (cdr atom)]
                        [new-id (if (eq? orig-id 'atom2)
                                    'bond-atom2
                                    (string->symbol (format "~a_~a" (symbol->string orig-id) (symbol->string 'frag2))))])
                   (hash-set! atom-map orig-id new-id)
                   (set-atom m new-id atom-value)))]
            [m (for/fold ([m m])
                         ([b (mol-bonds m1)])
                 (add-bond m b))]
            [m (for/fold ([m m])
                         ([b (mol-bonds m2)])
                 (let ([a1 (hash-ref atom-map (bond-a1 b))]
                       [a2 (hash-ref atom-map (bond-a2 b))])
                   (add-bond m (bond a1 a2 (bond-order b) (bond-stereo b)))))]
            [m (add-bond m (bond 'bond-atom1 'bond-atom2 order #f))])
       m)])

(define-syntax-parser combine-into-fragment
  [(_ name:id
      [frag1:id #:at atom1:id]
      [frag2:id #:at atom2:id]
      #:bt [bond-atom1:id bond-atom2:id order:number]
      #:exposed exposed-atom:id)
   #'(let* ([f1 frag1]
            [f2 frag2]
            [m1 (first f1)]
            [m2 (first f2)]
            [exposed1 (second f1)]
            [exposed2 (second f2)]
            [m (mol (make-immutable-hash) empty)]
            [m (for/fold ([m m]) ([atom (hash->list (mol-atoms m1))])
                 (set-atom m (car atom) (cdr atom)))]
            [atom-map (make-hash)]
            [m (for/fold ([m m]) ([atom (hash->list (mol-atoms m2))])
                 (let* ([orig-id (car atom)]
                        [atom-value (cdr atom)]
                        [new-id (if (eq? orig-id 'atom2)
                                    'bond-atom2
                                    (string->symbol (format "~a_~a" (symbol->string orig-id) (symbol->string 'frag2))))])
                   (hash-set! atom-map orig-id new-id)
                   (set-atom m new-id atom-value)))]
            [m (for/fold ([m m]) ([b (mol-bonds m1)])
                 (add-bond m b))]
            [m (for/fold ([m m])
                         ([b (mol-bonds m2)])
                 (let ([a1 (hash-ref atom-map (bond-a1 b))]
                       [a2 (hash-ref atom-map (bond-a2 b))])
                   (add-bond m (bond a1 a2 (bond-order b) (bond-stereo b)))))]
            [m (add-bond m (bond 'bond-atom1 'bond-atom2 order #f))])
       (list m (list 'exposed-atom)))])

(module+ test
  (require rackunit)

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

  (define f-methyl (first methyl))
  (define f-hydroxyl (first hydroxyl))

  (check-equal? (hash-count (mol-atoms f-methyl)) 4)
  (check-equal? (length (mol-bonds f-methyl)) 3)

  (check-equal? (hash-count (mol-atoms f-hydroxyl)) 2)
  (check-equal? (length (mol-bonds f-hydroxyl)) 1)

  (define water
    (molecule water
              (atoms [O O] [H1 H] [H2 H])
              (bonds [O H1 1] [O H2 1])))

  (check-equal? (hash-count (mol-atoms water)) 3)
  (check-equal? (length (mol-bonds water)) 2)

  ; Test combine-fragments
  (define methanol-combined
    (first (combine-fragments methanol-combined
                              [methyl #:at C1]
                              [hydroxyl #:at O1]
                              #:bt [C1 O1 1])))

  ; Check that methanol has the right structure
  (check-equal? (hash-count (mol-atoms methanol-combined)) 6)
  (check-equal? (length (mol-bonds methanol-combined)) 5)

  (define ethyl
    (fragment ethyl
              (atoms [C1 C] [C2 C] [H1 H] [H2 H] [H3 H] [H4 H] [H5 H])
              (bonds [C1 C2 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [C2 H4 1] [C2 H5 1])
              #:exposed C2))

  (define ethanol-combined
    (first (combine-fragments ethanol-combined
                              [ethyl #:at C2]
                              [hydroxyl #:at O1]
                              #:bt [C2 O1 1])))

  (check-equal? (hash-count (mol-atoms ethanol-combined)) 9)
  (check-equal? (length (mol-bonds ethanol-combined)) 8)

  (define carboxyl
    (fragment carboxyl
              (atoms [C C] [O1 O] [O2 O] [H H])
              (bonds [C O1 2] [C O2 1] [O2 H 1])
              #:exposed C))

  (define acetic-acid-combined
    (first (combine-fragments acetic-acid-combined
                              [methyl #:at C1]
                              [carboxyl #:at C]
                              #:bt [C1 C 1])))

  (check-equal? (hash-count (mol-atoms acetic-acid-combined)) 8)
  (check-equal? (length (mol-bonds acetic-acid-combined)) 7)

  ; FIXME: this is broken, uncomment these tests etc
  ; Test the ring-molecule helper
  ; (define benzene-ring
  ;   (ring-molecule benzene-ring 6 C))
  ;
  ; ; Check that benzene has the right structure
  ; (check-equal? (hash-count (mol-atoms benzene-ring)) 6)
  ; (check-equal? (length (mol-bonds benzene-ring)) 6)

  ; Test ring-molecule with additional atoms and bonds
  ; (define cyclohexanol
  ;   (ring-molecule cyclohexanol
  ;                 6
  ;                 C
  ;                 #:additional-atoms (list 'O1)
  ;                 #:additional-bonds (list (list 'C1 'O1 1))))
  ;
  ; ; Check that cyclohexanol has the right structure
  ; (check-equal? (hash-count (mol-atoms cyclohexanol)) 7)
  ; (check-equal? (length (mol-bonds cyclohexanol)) 7)

  ; Complete tests for the macrocyclic-ring helper too
  ; (define erythronolide-core
  ;   (macrolide-ring erythronolide-core
  ;                   #:ring-size 14
  ;                   #:o-positions (list 1 3 5 7 9 11 13)))
  ;
  ; (define frag-erythronolide (first erythronolide-core))
  ;
  ; ; Check that erythronolide has the right structure
  ; (check-true (> (hash-count (mol-atoms frag-erythronolide)) 20))
  ; (check-true (> (length (mol-bonds frag-erythronolide)) 20))
  )
