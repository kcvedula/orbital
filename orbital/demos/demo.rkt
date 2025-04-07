#lang racket

(require "../private/core-render.rkt"
         "../private/explore.rkt")


(define pid (case 5
              [(0) 4000]
              [(1) 4001]
              [(2) 3664]
              [(3) 45138674]
              [(4) 5997] ;; Cholesterol
              [(5) 5957] ;; Adenosine Triphosphate (ATP)
              ))

(parameterize ([FPS 60]
               [DELTA-LOOK (degrees->radians 0.75)]
               [DELTA-MOVE 1/8]
               [FOV 60])
  (explore-pid pid))
