#lang racket

(require pict3d)
(require racket/draw)
(require "explore.rkt")

(define c1
            (combine
             (cube origin 1)
             (basis 'R  (point-at  (pos 0 0 1) +z))))

(define c2 
            (combine
             (cube origin 1)
             (basis 'R  (point-at (pos 0 0 1) -z))))

(define c3 (replace-group c2 '(R) (λ (p) (rotate-z p 60))))

(explore (glue c1 '(R) c2 '(R)))
(explore (glue c1 '(R) c3 '(R)))


 
