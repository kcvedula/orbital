#lang racket

(require "../private/core.rkt"
         "../private/core-render.rkt"
         "../private/explore.rkt"
         "../private/molecular-helpers.rkt")

; Define a simple benzene molecule
(define benzene (make-ring-molecule 'benzene 6))

; Define a simple ethanol molecule - manually specifying the atoms and bonds
(define ethanol 
  (let* ([m (mol (make-immutable-hash) empty)]
         ; Add carbon atoms
         [m (set-atom m 'C1 (atom 'C #f #f #f))]
         [m (set-atom m 'C2 (atom 'C #f #f #f))]
         ; Add oxygen atom
         [m (set-atom m 'O1 (atom 'O #f #f #f))]
         ; Add hydrogen atoms
         [m (set-atom m 'H1 (atom 'H #f #f #f))]
         [m (set-atom m 'H2 (atom 'H #f #f #f))]
         [m (set-atom m 'H3 (atom 'H #f #f #f))]
         [m (set-atom m 'H4 (atom 'H #f #f #f))]
         [m (set-atom m 'H5 (atom 'H #f #f #f))]
         ; Add bonds
         [m (add-bond m (bond 'C1 'C2 1 #f))]
         [m (add-bond m (bond 'C2 'O1 1 #f))]
         [m (add-bond m (bond 'C1 'H1 1 #f))]
         [m (add-bond m (bond 'C1 'H2 1 #f))]
         [m (add-bond m (bond 'C1 'H3 1 #f))]
         [m (add-bond m (bond 'O1 'H4 1 #f))]
         [m (add-bond m (bond 'C2 'H5 1 #f))])
    m))

; Create the erythronolide core ring
(define erythronolide (make-macrolide 'erythronolide))

; Create a naphthalene structure (two fused benzene rings)
(define naphthalene
  (let* ([ring1-atoms (for/list ([i (in-range 1 7)])
                        (string->symbol (format "C~a" i)))]
         [ring2-atoms (for/list ([i (in-range 7 11)])
                        (string->symbol (format "C~a" i)))]
         [m (mol (make-immutable-hash) empty)]
         ; Add all carbon atoms
         [m (for/fold ([m m])
                     ([atom-id (append ring1-atoms ring2-atoms)])
              (set-atom m atom-id (atom 'C #f #f #f)))]
         ; Add first ring bonds
         [m (for/fold ([m m])
                     ([i (in-range (length ring1-atoms))])
              (add-bond m (bond (list-ref ring1-atoms i)
                               (list-ref ring1-atoms (modulo (add1 i) (length ring1-atoms)))
                               1 #f)))]
         ; Add second ring bonds - connecting to the shared atoms (C1 and C6)
         [m (add-bond m (bond 'C7 'C1 1 #f))]
         [m (add-bond m (bond 'C7 'C8 1 #f))]
         [m (add-bond m (bond 'C8 'C9 1 #f))]
         [m (add-bond m (bond 'C9 'C10 1 #f))]
         [m (add-bond m (bond 'C10 'C6 1 #f))])
    m))

; Create a steroid-like structure with 4 rings
(define steroid
  (let* ([m (mol (make-immutable-hash) empty)]
         ; Create the four rings (A, B, C, D from left to right)
         ; Ring A - 6-membered
         [ring-a-atoms (for/list ([i (in-range 1 7)])
                         (string->symbol (format "A~a" i)))]
         ; Ring B - 6-membered
         [ring-b-atoms (for/list ([i (in-range 1 7)])
                         (string->symbol (format "B~a" i)))]
         ; Ring C - 6-membered
         [ring-c-atoms (for/list ([i (in-range 1 7)])
                         (string->symbol (format "C~a" i)))]
         ; Ring D - 5-membered
         [ring-d-atoms (for/list ([i (in-range 1 6)])
                         (string->symbol (format "D~a" i)))]
         
         ; Add all carbon atoms
         [m (for/fold ([m m])
                     ([atom-id (append ring-a-atoms 
                                      (list 'B1 'B2 'B3 'B4) 
                                      (list 'C1 'C2 'C3 'C4) 
                                      ring-d-atoms)])
              (set-atom m atom-id (atom 'C #f #f #f)))]
         
         ; Add bonds for each ring
         ; Ring A bonds
         [m (for/fold ([m m])
                     ([i (in-range (length ring-a-atoms))])
              (add-bond m (bond (list-ref ring-a-atoms i)
                               (list-ref ring-a-atoms (modulo (add1 i) (length ring-a-atoms)))
                               1 #f)))]
         
         ; Ring B bonds - sharing A4 and A5 atoms with ring A
         [m (add-bond m (bond 'B1 'A4 1 #f))]
         [m (add-bond m (bond 'B1 'B2 1 #f))]
         [m (add-bond m (bond 'B2 'B3 1 #f))]
         [m (add-bond m (bond 'B3 'B4 1 #f))]
         [m (add-bond m (bond 'B4 'A5 1 #f))]
         
         ; Ring C bonds - sharing B4 and A5 atoms with ring B
         [m (add-bond m (bond 'C1 'B4 1 #f))]
         [m (add-bond m (bond 'C1 'C2 1 #f))]
         [m (add-bond m (bond 'C2 'C3 1 #f))]
         [m (add-bond m (bond 'C3 'C4 1 #f))]
         [m (add-bond m (bond 'C4 'A5 1 #f))]
         
         ; Ring D bonds - sharing C4 and A5 atoms with ring C (5-membered)
         [m (add-bond m (bond 'D1 'C4 1 #f))]
         [m (add-bond m (bond 'D1 'D2 1 #f))]
         [m (add-bond m (bond 'D2 'D3 1 #f))]
         [m (add-bond m (bond 'D3 'D4 1 #f))]
         [m (add-bond m (bond 'D4 'A5 1 #f))]
         
         ; Add a hydroxyl group to D3 (characteristic of sterols)
         [m (set-atom m 'O1 (atom 'O #f #f #f))]
         [m (set-atom m 'H1 (atom 'H #f #f #f))]
         [m (add-bond m (bond 'D3 'O1 1 #f))]
         [m (add-bond m (bond 'O1 'H1 1 #f))]
         
         ; Add an alkyl side chain to D4
         [m (set-atom m 'S1 (atom 'C #f #f #f))]
         [m (set-atom m 'S2 (atom 'C #f #f #f))]
         [m (set-atom m 'S3 (atom 'C #f #f #f))]
         [m (add-bond m (bond 'D4 'S1 1 #f))]
         [m (add-bond m (bond 'S1 'S2 1 #f))]
         [m (add-bond m (bond 'S2 'S3 1 #f))])
    m))

; Function to explore any defined molecule
(define (demo molecule-id)
  (define molecule 
    (case molecule-id
      [(0) benzene]
      [(1) ethanol]
      [(2) erythronolide]
      [(3) naphthalene]
      [(4) steroid]
      [else benzene]))
  
  (parameterize ([FPS 60]
                 [DELTA-LOOK (degrees->radians 0.75)]
                 [DELTA-MOVE 1/8]
                 [FOV 60])
    (explore-mol molecule)))

; 0 - benzene
; 1 - ethanol
; 2 - erythronolide
; 3 - naphthalene (fused rings)
; 4 - steroid (complex 4-ring system)
(demo 2) 
