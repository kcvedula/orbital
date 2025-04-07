#lang racket

(require "private/core-render.rkt")
(require "private/explore.rkt")



(parameterize [(FPS 60)
               (DELTA-LOOK (degrees->radians 1))]
  (explore-pid 3664))

