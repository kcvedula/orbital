#lang racket

(require pict3d)
(require pict3d/universe)

(require "explore-anim.rkt")
(require "chair.rkt")
(require "functional-groups.rkt")
(require "orbital1.rkt")

(define (-Me/s t)
  (define TEMP (rotate-z -Me (* 36 (/ t 1000))))
  TEMP)

(define (my-anim t)
  (sp3 #:d1 (rotate-z -Me (* 36 (/ t 1000)))))

(explore-anim my-anim)
