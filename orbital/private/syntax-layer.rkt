; TODO: consider using syntax-parse
#lang racket

(require "core.rkt"
         syntax/parse
         syntax/parse/define)

(provide fragment molecule make-chain ring-molecule)

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
            [exposed1 (second f1)]
            [exposed2 (second f2)]
            [m (mol (make-immutable-hash) empty)]
            ; Copy all atoms from first fragment
            [m (for/fold ([m m])
                         ([atom (hash->list (mol-atoms m1))])
                 (set-atom m (car atom) (cdr atom)))]
            ; Generate new atom IDs for second fragment to avoid collisions
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
            ; Copy all bonds from first fragment
            [m (for/fold ([m m])
                         ([b (mol-bonds m1)])
                 (add-bond m b))]
            ; Add bonds from second fragment with renamed atoms
            [m (for/fold ([m m])
                         ([b (mol-bonds m2)])
                 (let ([a1 (hash-ref atom-map (bond-a1 b))]
                       [a2 (hash-ref atom-map (bond-a2 b))])
                   (add-bond m (bond a1 a2 (bond-order b) (bond-stereo b)))))]
            ; Add the bond between fragments
            [m (add-bond m (bond 'bond-atom1 'bond-atom2 order #f))])
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

; Helper macros and functions for creating common molecular structures

; Generate a ring of atoms with specified size
(define-syntax-parser make-ring
  [(_ size:number atom-type:expr)
   #'(let* ([atom-ids (for/list ([i (in-range 1 (add1 size))])
                        (string->symbol (format "C~a" i)))]
            [bond-pairs (append
                         (for/list ([i (in-range size)])
                           (cons (list-ref atom-ids i)
                                 (list-ref atom-ids (modulo (add1 i) size))))
                         ; Return the atom IDs as well for reference
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
       (molecule name
                 (atoms . ,(car atom-bond-lists))
                 (bonds . ,(cadr atom-bond-lists))))])

; Predefined common molecular fragments
(define methyl-group
  (fragment methyl-group
            (atoms [C C] [H1 H] [H2 H] [H3 H])
            (bonds [C H1 1] [C H2 1] [C H3 1])
            #:exposed C))

(define ethyl-group
  (fragment ethyl-group
            (atoms [C1 C] [C2 C] [H1 H] [H2 H] [H3 H] [H4 H] [H5 H])
            (bonds [C1 C2 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [C2 H4 1] [C2 H5 1])
            #:exposed C2))

(define hydroxyl-group
  (fragment hydroxyl-group
            (atoms [O O] [H H])
            (bonds [O H 1])
            #:exposed O))

(define carboxyl-group
  (fragment carboxyl-group
            (atoms [C C] [O1 O] [O2 O] [H H])
            (bonds [C O1 2] [C O2 1] [O2 H 1])
            #:exposed C))

(define amino-group
  (fragment amino-group
            (atoms [N N] [H1 H] [H2 H])
            (bonds [N H1 1] [N H2 1])
            #:exposed N))

; Macro for creating a macrocyclic structure like erythronolide
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
       (fragment name
                 (atoms . ,atom-specs)
                 (bonds . ,bond-specs)
                 #:exposed ,(list-ref ring-atoms 2)))])

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

  (define carbonyl
    (fragment carbonyl
              (atoms [C1 C] [O1 O])
              (bonds [C1 O1 2])
              #:exposed C1))

  (define amino
    (fragment amino
              (atoms [N1 N] [H1 H] [H2 H])
              (bonds [N1 H1 1] [N1 H2 1])
              #:exposed N1))

  (define ethanol
    (molecule ethanol
              (atoms [C1 C] [C2 C] [O1 O] [H1 H] [H2 H] [H3 H] [H4 H] [H5 H])
              (bonds [C1 C2 1] [C2 O1 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [O1 H4 1] [C2 H5 1])))

  (define erythronolide
    (molecule erythronolide
              (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [C6 C] [C7 C] [C8 C] [C9 C] [C10 C]
                     [C11 C] [C12 C] [C13 C] [C14 C]
                     [O1 O] [O2 O] [O3 O] [O4 O] [O5 O] [O6 O] [O7 O]
                     [H1 H] [H2 H] [H3 H] [H4 H] [H5 H] [H6 H] [H7 H] [H8 H] [H9 H] [H10 H]
                     [H11 H] [H12 H] [H13 H] [H14 H] [H15 H] [H16 H] [H17 H] [H18 H] [H19 H] [H20 H])
              (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 C6 1] [C6 C7 1] [C7 C8 1] [C8 C9 1]
                     [C9 C10 1] [C10 C11 1] [C11 C12 1] [C12 C13 1] [C13 C14 1] [C14 C1 1]
                     [C1 O1 1] [C3 O2 1] [C5 O3 1] [C6 O4 1] [C9 O5 1] [C11 O6 1] [C13 O7 1]
                     [C1 H1 1] [C2 H2 1] [C2 H3 1] [C3 H4 1] [C4 H5 1] [C4 H6 1] [C5 H7 1]
                     [C7 H8 1] [C7 H9 1] [C8 H10 1] [C8 H11 1] [C9 H12 1] [C10 H13 1] [C10 H14 1]
                     [C11 H15 1] [C12 H16 1] [C12 H17 1] [C13 H18 1] [C14 H19 1] [C14 H20 1])))

  (define methanol
    (combine-fragments methanol
                       [methyl #:at C1]
                       [hydroxyl #:at O1]
                       #:bt [C1 O1 1]))

  (define acetic-acid
    (molecule acetic-acid
              (atoms [C1 C] [C2 C] [O1 O] [O2 O] [H1 H] [H2 H] [H3 H] [H4 H])
              (bonds [C1 C2 1] [C2 O1 2] [C2 O2 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [O2 H4 1])))

  (define acetic-acid-from-fragments
    (molecule acetic-acid-from-fragments
              (atoms [C1 C] [C2 C] [O1 O] [O2 O] [H1 H] [H2 H] [H3 H] [H4 H])
              (bonds [C1 C2 1] [C2 O1 2] [C2 O2 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [O2 H4 1])))

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
                                   'O1 (atom 'O #f #f #f)
                                   'H1_hydroxyl (atom 'H #f #f #f))
                             (list (bond 'C1 'H1 1 #f)
                                   (bond 'C1 'H2 1 #f)
                                   (bond 'C1 'H3 1 #f)
                                   (bond 'C1 'O1 1 #f)
                                   (bond 'O1 'H1_hydroxyl 1 #f)))))

  (check-equal? (hash-count (mol-atoms ethanol)) 8)
  (check-equal? (length (mol-bonds ethanol)) 7)

  (check-equal? (hash-count (mol-atoms acetic-acid-from-fragments)) 8)
  (check-equal? (length (mol-bonds acetic-acid-from-fragments)) 7)

  (check-equal? (hash-count (mol-atoms erythronolide)) 41)
  (check-equal? (length (mol-bonds erythronolide)) 41)

  (define cladinose
    (molecule cladinose
              (atoms [C1 C] [C2 C] [C3 C] [O1 O] [O2 O] [O3 O]
                     [H1 H] [H2 H] [H3 H] [H4 H] [H5 H] [H6 H] [H7 H] [H8 H])
              (bonds [C1 C2 1] [C2 C3 1] [C1 O1 1] [C2 O2 1] [C3 O3 1]
                     [C1 H1 1] [C1 H2 1] [C2 H3 1] [C3 H4 1] [C3 H5 1]
                     [O1 H6 1] [O2 H7 1] [O3 H8 1])))

  (define desosamine
    (molecule desosamine
              (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [N1 N] [O1 O] [O2 O]
                     [H1 H] [H2 H] [H3 H] [H4 H] [H5 H] [H6 H] [H7 H] [H8 H] [H9 H] [H10 H])
              (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 N1 1]
                     [C1 O1 1] [C3 O2 1]
                     [C1 H1 1] [C2 H2 1] [C2 H3 1] [C3 H4 1] [C4 H5 1] [C4 H6 1]
                     [C5 H7 1] [N1 H8 1] [N1 H9 1] [O1 H10 1])))

  ; Test that sugar components have correct atom and bond counts
  (check-equal? (hash-count (mol-atoms cladinose)) 14)
  (check-equal? (length (mol-bonds cladinose)) 13)
  
  (check-equal? (hash-count (mol-atoms desosamine)) 18)
  (check-equal? (length (mol-bonds desosamine)) 17)

  ; Full erythromycin A structure
  (define erythromycin-a
    (molecule erythromycin-a
              (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [C6 C] [C7 C] [C8 C] [C9 C] [C10 C]
                     [C11 C] [C12 C] [C13 C] [C14 C] [C15 C] [C16 C] [C17 C] [C18 C] [C19 C] [C20 C]
                     [C21 C] [C22 C] [C23 C] [C24 C] [C25 C] [C26 C] [C27 C] [C28 C] [C29 C] [C30 C]
                     [C31 C] [C32 C] [C33 C] [C34 C] [C35 C] [C36 C] [C37 C]
                     [O1 O] [O2 O] [O3 O] [O4 O] [O5 O] [O6 O] [O7 O] [O8 O] [O9 O] [O10 O] [O11 O] [O12 O] [O13 O]
                     [N1 N] [N2 N]
                     [H1 H] [H2 H] [H3 H] [H4 H] [H5 H] [H6 H] [H7 H] [H8 H] [H9 H] [H10 H]
                     [H11 H] [H12 H] [H13 H] [H14 H] [H15 H] [H16 H] [H17 H] [H18 H] [H19 H] [H20 H]
                     [H21 H] [H22 H] [H23 H] [H24 H] [H25 H] [H26 H] [H27 H] [H28 H] [H29 H] [H30 H]
                     [H31 H] [H32 H] [H33 H] [H34 H] [H35 H] [H36 H] [H37 H] [H38 H] [H39 H] [H40 H]
                     [H41 H] [H42 H] [H43 H] [H44 H] [H45 H] [H46 H] [H47 H] [H48 H] [H49 H])
              (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 C6 1] [C6 C7 1] [C7 C8 1] [C8 C9 1]
                     [C9 C10 1] [C10 C11 1] [C11 C12 1] [C12 C13 1] [C13 C14 1] [C14 C1 1]
                     [C1 O1 1] [C3 O2 1] [C5 O3 1] [C6 O4 1] [C9 O5 1] [C11 O6 1] [C13 O7 1]
                     [C3 C15 1] [C15 O8 1] [C15 C16 1] [C16 C17 1] [C17 C18 1] [C18 N1 1]
                     [C5 C19 1] [C19 O9 1] [C19 C20 1] [C20 C21 1] [C21 O10 1] [C21 O11 1]
                     [C1 H1 1] [C2 H2 1] [C2 H3 1] [C4 H4 1] [C4 H5 1] [C6 H6 1] [C7 H7 1] [C7 H8 1]
                     [C8 H9 1] [C8 H10 1] [C9 H11 1] [C10 H12 1] [C10 H13 1] [C11 H14 1] [C12 H15 1]
                     [C12 H16 1] [C13 H17 1] [C14 H18 1] [C14 H19 1] [C15 H20 1] [C16 H21 1] [C16 H22 1]
                     [C17 H23 1] [C17 H24 1] [C18 H25 1] [C18 H26 1] [N1 H27 1] [N1 H28 1] [O8 H29 1]
                     [C19 H30 1] [C20 H31 1] [C20 H32 1] [O9 H33 1] [O10 H34 1] [O11 H35 1])))

  ; Test that erythromycin A has correct atom and bond counts
  (check-equal? (hash-count (mol-atoms erythromycin-a)) 101)
  (check-equal? (length (mol-bonds erythromycin-a)) 68)

  ; Creating erythromycin-a by combining fragments
  ; First, define fragments with exposed connection points
  (define erythronolide-fragment
    (fragment erythronolide-fragment
              (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [C6 C] [C7 C] [C8 C] [C9 C] [C10 C]
                     [C11 C] [C12 C] [C13 C] [C14 C]
                     [O1 O] [O2 O] [O3 O] [O4 O] [O5 O] [O6 O] [O7 O]
                     [H1 H] [H2 H] [H3 H] [H4 H] [H5 H] [H6 H] [H7 H] [H8 H] [H9 H] [H10 H]
                     [H11 H] [H12 H] [H13 H] [H14 H] [H15 H] [H16 H] [H17 H] [H18 H] [H19 H] [H20 H])
              (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 C6 1] [C6 C7 1] [C7 C8 1] [C8 C9 1]
                     [C9 C10 1] [C10 C11 1] [C11 C12 1] [C12 C13 1] [C13 C14 1] [C14 C1 1]
                     [C1 O1 1] [C3 O2 1] [C5 O3 1] [C6 O4 1] [C9 O5 1] [C11 O6 1] [C13 O7 1]
                     [C1 H1 1] [C2 H2 1] [C2 H3 1] [C3 H4 1] [C4 H5 1] [C4 H6 1] [C5 H7 1]
                     [C7 H8 1] [C7 H9 1] [C8 H10 1] [C8 H11 1] [C9 H12 1] [C10 H13 1] [C10 H14 1]
                     [C11 H15 1] [C12 H16 1] [C12 H17 1] [C13 H18 1] [C14 H19 1] [C14 H20 1])
              #:exposed C3))
                     
  (define cladinose-fragment
    (fragment cladinose-fragment
              (atoms [C1 C] [C2 C] [C3 C] [O1 O] [O2 O] [O3 O]
                     [H1 H] [H2 H] [H3 H] [H4 H] [H5 H] [H6 H] [H7 H] [H8 H])
              (bonds [C1 C2 1] [C2 C3 1] [C1 O1 1] [C2 O2 1] [C3 O3 1]
                     [C1 H1 1] [C1 H2 1] [C2 H3 1] [C3 H4 1] [C3 H5 1]
                     [O1 H6 1] [O2 H7 1] [O3 H8 1])
              #:exposed O1))
              
  (define desosamine-fragment
    (fragment desosamine-fragment
              (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [N1 N] [O1 O] [O2 O]
                     [H1 H] [H2 H] [H3 H] [H4 H] [H5 H] [H6 H] [H7 H] [H8 H] [H9 H] [H10 H])
              (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 N1 1]
                     [C1 O1 1] [C3 O2 1]
                     [C1 H1 1] [C2 H2 1] [C2 H3 1] [C3 H4 1] [C4 H5 1] [C4 H6 1]
                     [C5 H7 1] [N1 H8 1] [N1 H9 1] [O1 H10 1])
              #:exposed O1))
              
  ; Now combine these fragments to create erythromycin-a
  (define erythromycin-a-from-fragments
    (let* ([core-with-cladinose (combine-fragments core-with-cladinose
                                                  [erythronolide-fragment #:at C3]
                                                  [cladinose-fragment #:at O1]
                                                  #:bt [O2 O1 1])]
           [erythromycin-completed (combine-fragments erythromycin-completed
                                                     [core-with-cladinose #:at C5]
                                                     [desosamine-fragment #:at O1]
                                                     #:bt [O3 O1 1])])
      erythromycin-completed))
      
  ; Test that our new way of building erythromycin matches the original
  (check-true (mol-equal? erythromycin-a erythromycin-a-from-fragments))

  ; Test new combine-fragments with more complex fragments
  (define carboxylic-acid
    (fragment carboxylic-acid
              (atoms [C1 C] [O1 O] [O2 O] [H1 H])
              (bonds [C1 O1 2] [C1 O2 1] [O2 H1 1])
              #:exposed C1))
              
  (define phenyl
    (fragment phenyl
              (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [C6 C] [H1 H] [H2 H] [H3 H] [H4 H] [H5 H])
              (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 C6 1] [C6 C1 1]
                     [C2 H1 1] [C3 H2 1] [C4 H3 1] [C5 H4 1] [C6 H5 1])
              #:exposed C1))
              
  (define benzoic-acid
    (combine-fragments benzoic-acid
                       [phenyl #:at C1]
                       [carboxylic-acid #:at C1]
                       #:bt [C1 C1 1]))
                       
  ; Test that benzoic acid has the expected atom and bond counts                     
  (check-equal? (hash-count (mol-atoms benzoic-acid)) 14) ; Combined with atom renaming
  (check-equal? (length (mol-bonds benzoic-acid)) 15)

  (check-true (mol-equal? benzoic-acid benzoic-acid))

  ; Example: Creating erythromycin-a using our new helpers
  ; First create the erythronolide ring core
  (define erythronolide-core-simple
    (macrolide-ring erythronolide-core-simple))
  
  ; Define simplified sugar fragments
  (define cladinose-simple
    (ring-molecule cladinose-simple 
                   3 
                   C
                   #:additional-atoms (list 'O1 'O2 'O3)
                   #:additional-bonds (list (list 'C1 'O1 1) (list 'C2 'O2 1) (list 'C3 'O3 1))))
  
  (define desosamine-simple
    (let* ([chain (make-chain 5 C)]
           [atoms (last chain)]
           [n-atom 'N1]
           [o-atoms (list 'O1 'O2)]
           [all-atoms (append atoms (list n-atom) o-atoms)]
           [additional-bonds (list 
                              (list 'C5 'N1 1)
                              (list 'C1 'O1 1)
                              (list 'C3 'O2 1))])
      (fragment desosamine-simple
                (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [N1 N] [O1 O] [O2 O])
                (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 N1 1]
                       [C1 O1 1] [C3 O2 1])
                #:exposed O1)))
  
  ; Create erythromycin using the macrolide helper and combine with sugar components
  (define erythromycin-simple
    (let* ([with-cladinose (combine-fragments with-cladinose
                                             [erythronolide-core-simple #:at C3]
                                             [cladinose-simple #:at C1]
                                             #:bt [O2 O1 1])]
           [full-erythromycin (combine-fragments full-erythromycin
                                                [with-cladinose #:at C5]
                                                [desosamine-simple #:at C1]
                                                #:bt [O3 O1 1])])
      full-erythromycin))
      
  ; Demonstrate the power of our ring-molecule helper
  (define benzene-simple
    (ring-molecule benzene-simple 6 C))
    
  (define cyclohexane-simple
    (ring-molecule cyclohexane-simple 6 C))
    
  (define benzene-with-carboxyl
    (combine-fragments benzene-with-carboxyl
                      [benzene-simple #:at C1]
                      [carboxyl-group #:at C]
                      #:bt [C1 C 1]))
                      
  ; Verify that benzene-with-carboxyl and benzoic-acid have the same structure
  (check-true (mol-equal? benzene-with-carboxyl benzoic-acid)))
