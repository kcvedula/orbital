#lang racket

(require "../private/core.rkt"
         "../private/core-render.rkt"
         "../private/explore.rkt"
         "../private/syntax-layer.rkt")

(provide fragment molecule make-chain ring-molecule combine-fragments)

(define benzene 
  (molecule benzene
            (atoms [C1 C] [C2 C] [C3 C] [C4 C] [C5 C] [C6 C])
            (bonds [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 C6 1] [C6 C1 1])))
            
(define ethanol
  (molecule ethanol
            (atoms [C1 C] [C2 C] [O1 O] [H1 H] [H2 H] [H3 H] [H4 H] [H5 H])
            (bonds [C1 C2 1] [C2 O1 1] [C1 H1 1] [C1 H2 1] [C1 H3 1] [O1 H4 1] [C2 H5 1])))

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

(define naphthalene 
  (combine-fragments naphthalene
                    [naphthalene-fragment1 #:at C1]
                    [naphthalene-fragment2 #:at C7]
                    #:bt [C1 C7 1]))

(define steroid-core-d3
  (fragment steroid-core-d3
            (atoms 
             [A1 C] [A2 C] [A3 C] [A4 C] [A5 C] [A6 C] ; Ring A
             [B1 C] [B2 C] [B3 C] [B4 C] [B5 C]        ; Ring B 
             [C1 C] [C2 C] [C3 C] [C4 C] [C5 C]        ; Ring C
             [D1 C] [D2 C] [D3 C] [D4 C] [D5 C])       ; Ring D
            (bonds
             [A1 A2 1] [A2 A3 1] [A3 A4 1] [A4 A5 1] [A5 A6 1] [A6 A1 1] ; Ring A bonds
             [B1 A4 1] [B1 B2 1] [B2 B3 1] [B3 B4 1] [B4 B5 1] [B5 A5 1] ; Ring B bonds: sharing A4 A5
             [C1 B4 1] [C1 C2 1] [C2 C3 1] [C3 C4 1] [C4 C5 1] [C5 B5 1] ; Ring C bonds: sharing B4 B5
             [D1 C4 1] [D1 D2 1] [D2 D3 1] [D3 D4 1] [D4 D5 1] [D5 C5 1]); Ring D bonds: sharing C4 C5
            #:exposed D3))

(define hydroxyl
  (fragment hydroxyl
            (atoms [O1 O] [H1 H])
            (bonds [O1 H1 1])
            #:exposed O1))

(define alkyl-chain
  (fragment alkyl-chain
            (atoms [S1 C] [S2 C] [S3 C])
            (bonds [S1 S2 1] [S2 S3 1])
            #:exposed S1))

(define steroid-with-hydroxyl
  (combine-into-fragment steroid-with-hydroxyl
                       [steroid-core-d3 #:at D3]
                       [hydroxyl #:at O1]
                       #:bt [D3 O1 1]
                       #:exposed D4))

(define steroid
  (combine-fragments steroid
                     [steroid-with-hydroxyl #:at D4]
                     [alkyl-chain #:at S1]
                     #:bt [D4 S1 1]))

(define (demo molecule-id)
  (define molecule 
    (case molecule-id
      [(0) benzene]
      [(1) ethanol]
      [(2) naphthalene]
      [(3) steroid]))
  
  (parameterize ([FPS 60]
                 [DELTA-LOOK (degrees->radians 0.75)]
                 [DELTA-MOVE 1/8]
                 [FOV 60])
    (explore-mol molecule)))

; 0 - benzene (simple ring)
; 1 - ethanol (simple molecule with syntax layer)
; 2 - naphthalene (combined fragments)
; 3 - steroid (complex molecule with combined fragments)
(demo 3) 
