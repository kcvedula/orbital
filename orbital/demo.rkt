#lang racket

(require "private/core-render.rkt")
(require "private/explore.rkt")



(parameterize [(FPS 60)
               (DELTA-LOOK (degrees->radians 0.75))]
  ; (explore-pid 3664)
  (explore-pid 4001))

