#lang info
(define collection "orbital")
(define deps '("base" "pict3d" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/orbital.scrbl" ())))
(define pkg-desc "3D Molecular Rendering")
(define version "0.1")
(define pkg-authors '(kcvedula ironmoon))
(define license '(GPL-3.0-or-later))
