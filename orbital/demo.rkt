#lang racket

(require "private/core-render.rkt")
(require "private/explore.rkt")


(define pid (case 0
              [(0) 4000]
              [(1) 4001]
              [(2) 3664]))

(parameterize ([FPS 60]
               [DELTA-LOOK (degrees->radians 0.75)]
               [DELTA-MOVE 1/8]
               [FOV 60])
  (explore-pid pid))
