#lang racket

(require "private/core.rkt"
         "private/core-render.rkt"
         "private/explore.rkt"
         "private/syntax-layer.rkt")

(provide fragment molecule make-chain ring-molecule combine-fragments)

; Define a simple benzene molecule using the syntax layer
(define benzene 
  (molecule benzene
            (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [C6 C])
            (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 C6 1] [C6 C1 1])))
            
; Define ethanol using the syntax layer
(define ethanol
  (molecule ethanol
            (atoms [C1 C] [C2 C] [O1 O] [H1 H] [H2 H] [H3 H] [H4 H] [H5 H])
            (bonds [C1 C2 1] [C2 O1 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [O1 H4 1] [C2 H5 1])))

; Define fragments for naphthalene
(define naphthalene-fragment1
  (fragment naphthalene1
            (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [C6 C])
            (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 C6 1] [C6 C1 1])
            #:exposed C1))

(define naphthalene-fragment2
  (fragment naphthalene2
            (atoms [C7 C] [C8 C] [C9 C] [C10 C])
            (bonds [C7 C8 1] [C8 C9 1] [C9 C10 1])
            #:exposed C7))

; Combine fragments to create naphthalene
(define naphthalene 
  (combine-fragments naphthalene
                    [naphthalene-fragment1 #:at C1]
                    [naphthalene-fragment2 #:at C7]
                    #:bt [C1 C7 1]))

; Define a more complex molecule using the syntax layer
(define steroid-core
  (molecule steroid-core
            (atoms 
             ; Ring A
             [A1 C] [A2 C] [A3 C] [A4 C] [A5 C] [A6 C]
             ; Ring B 
             [B1 C] [B2 C] [B3 C] [B4 C] [B5 C]
             ; Ring C
             [C1 C] [C2 C] [C3 C] [C4 C] [C5 C]
             ; Ring D
             [D1 C] [D2 C] [D3 C] [D4 C] [D5 C])
            (bonds
             ; Ring A bonds
             [A1 A2 1] [A2 A3 1] [A3 A4 1] [A4 A5 1] [A5 A6 1] [A6 A1 1]
             ; Ring B bonds - sharing A4 and A5
             [B1 A4 1] [B1 B2 1] [B2 B3 1] [B3 B4 1] [B4 B5 1] [B5 A5 1]
             ; Ring C bonds - sharing B4 and B5
             [C1 B4 1] [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 B5 1]
             ; Ring D bonds - sharing C4 and C5
             [D1 C4 1] [D1 D2 1] [D2 D3 1] [D3 D4 1] [D4 C5 1])))

; Define hydroxyl fragment
(define hydroxyl
  (fragment hydroxyl
            (atoms [O1 O] [H1 H])
            (bonds [O1 H1 1])
            #:exposed O1))

; Define alkyl chain fragment
(define alkyl-chain
  (fragment alkyl-chain
            (atoms [S1 C] [S2 C] [S3 C])
            (bonds [S1 S2 1] [S2 S3 1])
            #:exposed S1))

; Combine the steroid core with functional groups
(define steroid
  (let* ([core steroid-core]
         ; First add the hydroxyl group to D3
         [with-hydroxyl (set-atom core 'O1 (atom 'O #f #f #f))]
         [with-hydroxyl (set-atom with-hydroxyl 'H1 (atom 'H #f #f #f))]
         [with-hydroxyl (add-bond with-hydroxyl (bond 'D3 'O1 1 #f))]
         [with-hydroxyl (add-bond with-hydroxyl (bond 'O1 'H1 1 #f))]
         ; Then add the alkyl chain to D4
         [with-chain (set-atom with-hydroxyl 'S1 (atom 'C #f #f #f))]
         [with-chain (set-atom with-chain 'S2 (atom 'C #f #f #f))]
         [with-chain (set-atom with-chain 'S3 (atom 'C #f #f #f))]
         [with-chain (add-bond with-chain (bond 'D4 'S1 1 #f))]
         [with-chain (add-bond with-chain (bond 'S1 'S2 1 #f))]
         [with-chain (add-bond with-chain (bond 'S2 'S3 1 #f))])
    with-chain))

; Function to explore any defined molecule
(define (demo molecule-id)
  (define molecule 
    (case molecule-id
      [(0) benzene]
      [(1) ethanol]
      [(2) naphthalene]
      [(3) steroid]
      [else benzene]))
  
  (parameterize ([FPS 60]
                 [DELTA-LOOK (degrees->radians 0.75)]
                 [DELTA-MOVE 1/8]
                 [FOV 60])
    (explore-mol molecule)))

; Start the demo with the selected molecule
; Change this number to explore different molecules:
; 0 - benzene (simple ring)
; 1 - ethanol (simple molecule with syntax layer)
; 2 - naphthalene (combined fragments)
; 3 - steroid (complex molecule with combined fragments)
(demo 2) 

