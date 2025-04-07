#lang racket

(require "core.rkt")

(provide make-ring 
         make-chain
         make-ring-molecule
         make-macrolide
         methyl-group
         hydroxyl-group
         carboxyl-group
         amino-group)

; Generate a ring of atoms with specified size
(define (make-ring size [atom-type 'C])
  (let* ([atom-ids (for/list ([i (in-range 1 (add1 size))])
                     (string->symbol (format "C~a" i)))]
         [bond-pairs (for/list ([i (in-range size)])
                       (cons (list-ref atom-ids i)
                             (list-ref atom-ids (modulo (add1 i) size))))])
    (values atom-ids bond-pairs)))
    
; Generate a linear chain of atoms
(define (make-chain length [atom-type 'C])
  (let* ([atom-ids (for/list ([i (in-range 1 (add1 length))])
                     (string->symbol (format "C~a" i)))]
         [bond-pairs (for/list ([i (in-range (sub1 length))])
                       (cons (list-ref atom-ids i)
                             (list-ref atom-ids (add1 i))))])
    (values atom-ids bond-pairs)))

; Create a complete ring molecule
(define (make-ring-molecule name size [atom-type 'C] 
                           #:additional-atoms [additional-atoms '()]
                           #:additional-bonds [additional-bonds '()])
  (let-values ([(ring-atoms ring-bonds) (make-ring size atom-type)])
    (let* ([m (mol (make-immutable-hash) empty)]
           ; Add all the carbon atoms in the ring
           [m (for/fold ([m m])
                        ([a ring-atoms])
                 (set-atom m a (atom atom-type #f #f #f)))]
           ; Add ring bonds
           [m (for/fold ([m m])
                        ([b ring-bonds])
                 (add-bond m (bond (car b) (cdr b) 1 #f)))]
           ; Add additional atoms
           [m (for/fold ([m m])
                        ([a-pair additional-atoms])
                 (set-atom m (car a-pair) (atom (cdr a-pair) #f #f #f)))]
           ; Add additional bonds
           [m (for/fold ([m m])
                        ([b additional-bonds])
                 (add-bond m (bond (first b) (second b) (third b) #f)))])
      m)))

; Create a macrolide-like structure with oxygen positions
(define (make-macrolide name 
                       #:ring-size [ring-size 14]
                       #:o-positions [o-positions '(1 3 5 6 9 11 13)])
  (let-values ([(ring-atoms ring-bonds) (make-ring ring-size 'C)])
    (let* ([oxygen-atoms (for/list ([pos o-positions])
                           (cons (string->symbol (format "O~a" pos)) 'O))]
           [additional-bonds (for/list ([o-pair oxygen-atoms]
                                        [pos o-positions])
                               (list (list-ref ring-atoms (sub1 pos)) 
                                     (car o-pair) 
                                     1))])
      (make-ring-molecule name ring-size 'C
                         #:additional-atoms oxygen-atoms
                         #:additional-bonds additional-bonds))))

; Predefined common molecular fragments
(define (methyl-group)
  (let* ([m (mol (make-immutable-hash) empty)]
         [m (set-atom m 'C (atom 'C #f #f #f))]
         [m (set-atom m 'H1 (atom 'H #f #f #f))]
         [m (set-atom m 'H2 (atom 'H #f #f #f))]
         [m (set-atom m 'H3 (atom 'H #f #f #f))]
         [m (add-bond m (bond 'C 'H1 1 #f))]
         [m (add-bond m (bond 'C 'H2 1 #f))]
         [m (add-bond m (bond 'C 'H3 1 #f))])
    (list m '(C))))

(define (hydroxyl-group)
  (let* ([m (mol (make-immutable-hash) empty)]
         [m (set-atom m 'O (atom 'O #f #f #f))]
         [m (set-atom m 'H (atom 'H #f #f #f))]
         [m (add-bond m (bond 'O 'H 1 #f))])
    (list m '(O))))

(define (carboxyl-group)
  (let* ([m (mol (make-immutable-hash) empty)]
         [m (set-atom m 'C (atom 'C #f #f #f))]
         [m (set-atom m 'O1 (atom 'O #f #f #f))]
         [m (set-atom m 'O2 (atom 'O #f #f #f))]
         [m (set-atom m 'H (atom 'H #f #f #f))]
         [m (add-bond m (bond 'C 'O1 2 #f))]
         [m (add-bond m (bond 'C 'O2 1 #f))]
         [m (add-bond m (bond 'O2 'H 1 #f))])
    (list m '(C))))

(define (amino-group)
  (let* ([m (mol (make-immutable-hash) empty)]
         [m (set-atom m 'N (atom 'N #f #f #f))]
         [m (set-atom m 'H1 (atom 'H #f #f #f))]
         [m (set-atom m 'H2 (atom 'H #f #f #f))]
         [m (add-bond m (bond 'N 'H1 1 #f))]
         [m (add-bond m (bond 'N 'H2 1 #f))])
    (list m '(N))))

(module+ test
  (require rackunit)
  
  ; Test ring creation
  (let-values ([(atoms bonds) (make-ring 6)])
    (check-equal? (length atoms) 6)
    (check-equal? (length bonds) 6))
  
  ; Test benzene creation
  (define benzene (make-ring-molecule 'benzene 6))
  (check-equal? (hash-count (mol-atoms benzene)) 6)
  (check-equal? (length (mol-bonds benzene)) 6)
  
  ; Test macrolide creation
  (define erythronolide-core (make-macrolide 'erythronolide))
  (check-equal? (hash-count (mol-atoms erythronolide-core)) 21) ; 14 C + 7 O
  (check-equal? (length (mol-bonds erythronolide-core)) 21) ; 14 ring bonds + 7 C-O bonds
  
  ; Test methyl group
  (define methyl (methyl-group))
  (check-equal? (hash-count (mol-atoms (first methyl))) 4) ; C + 3H
  (check-equal? (length (mol-bonds (first methyl))) 3) ; 3 C-H bonds
  
  ; Test carboxyl group
  (define carboxyl (carboxyl-group))
  (check-equal? (hash-count (mol-atoms (first carboxyl))) 4) ; C + 2O + H
  (check-equal? (length (mol-bonds (first carboxyl))) 3) ; C=O + C-O + O-H
) 
