#lang typed/racket

(require pict3d)

(provide (all-defined-out))

(define-type Sym/Int (U Integer Symbol))
(define-type Sym/Int/Str (U Sym/Int String))

(: sym/int->string (-> Sym/Int/Str String))
(define (sym/int->string sn)
  (match sn
    ((? integer?) (number->string sn))
    ((? symbol?) (symbol->string sn))
    ((? string?) sn)))

(: sym+ (-> Sym/Int/Str Sym/Int/Str Symbol))
(define (sym+ s1 s2)
  (string->symbol (string-append (sym/int->string s1) (sym/int->string s2))))

(: dirn (-> Real Real Real Dir))
(define (dirn dx dy dz)
  (cast (dir-normalize (dir dx dy dz)) Dir))
