#lang racket
(require "templates-and-substituents.rkt"
         "types.rkt"
         "periodic-table.rkt"
         racket/hash
         racket/struct
         syntax-spec-v3
         syntax/parse
         (for-syntax
          data/union-find
          syntax/parse
          syntax-spec-v3
          racket/string
          racket/match
          racket/math
          racket/contract
          racket/struct
          rackunit
          "types.rkt"
          "periodic-table.rkt"))

;-------------------------------------------------------------------------------
; Runtime Rendering for Testing during Development                             |
;-------------------------------------------------------------------------------
(require "render2d.rkt"
         "mol-to-cml.rkt"
         "babel.rkt")

(define (mol->pict m)
  (png->pict (babel (mol->cml m) png)))

;-------------------------------------------------------------------------------
; Procedures, Static Checks and Syntax for an-element-symbol                   |
;-------------------------------------------------------------------------------

(define-for-syntax (valid-aes? aes)
  (pair? (member aes (map element-symbol periodic-table))))

(define-for-syntax (check-aes! aes ctxt)
  (define help-msg "(map element-symbol periodic-table) for all acceptable values")
  (unless (valid-aes? aes)
    (raise-syntax-error
     'periodic-table
     (format "~v is not a known an-element-symbol\n~a" aes help-msg)
     ctxt)))

(define-syntax (get-an-element-symbol stx)
  (syntax-parse stx
    ((_ s)
     (define res (an-element-symbol (syntax->datum #'s)))
     (check-aes! res #'s)
     #`(an-element-symbol (syntax->datum #'s)))))

(define (valid-element-symbol-value? s)
  (pair? (member (an-element-symbol s)
                 (map element-symbol periodic-table))))

; ------------------------------------------------------------------------------
; Syntax Specification                                                         |
; ------------------------------------------------------------------------------

(begin-for-syntax
  (define-persistent-symbol-table st))

(syntax-spec
 (host-interface/expression
  (sketch-template
   #:atoms a:spec-atom ...
   #:bonds b:spec-bond ...)
  #:binding (scope (import a) ... b ...)
  #'(compile-sketch-template->template
     #:atoms a ...
     #:bonds b ...))

 (host-interface/definitions
  (define-sketch-template
    id:racket-var
    #:atoms a:spec-atom ...
    #:bonds b:spec-bond ...)
  #:binding [(export id) (scope (import a) ... b ...)]
  (begin
    (symbol-table-set! st
                       #'id
                       #'(compile-sketch-template->template
                          #:atoms a ...
                          #:bonds b ...))
    #'(begin (define id (compile-sketch-template->template
                         #:atoms a ...
                         #:bonds b ...)))))

 (nonterminal/exporting spec-atom
                        ; simple
                        id:racket-var
                        #:binding (export id)
                        ; complex
                        (id:racket-var
                         mass-number:racket-expr
                         chirality:racket-expr
                         formal-charge:racket-expr)
                        #:binding (export id))

 (nonterminal spec-bond
              ; simple
              (a1:racket-var a2:racket-var)
              ; complex
              (a1:racket-var
               a2:racket-var
               order:racket-expr
               stereo:racket-expr)))
;-------------------------------------------------------------------------------
; Examples                                                                     |
;-------------------------------------------------------------------------------

(define-sketch-template ex-1
  #:atoms
  C-1 C-2 C-3 C-4 C-5 C-6 C-7 C-8
  #:bonds
  (C-1 C-2 1 #f)
  (C-2 C-3)
  (C-3 C-4)
  (C-4 C-5)
  (C-5 C-6)
  (C-6 C-1)
  (C-1 C-7)
  (C-7 C-8)
  (C-8 C-4))

(define-syntax (foo id)
  (syntax-parse id
    ((_ id) (print (symbol-table-has-key? st #'id))))
  #'(void))

#;(foo ex-1) ; ERROR HERE



; we want to replicate this behavior with:

#;(define ex-2.2
    (extend-template
     ex-1
     #:atoms C-9
     #:bonds (C-1 C-9)))
; maybe by doing define-template and define-template/extend

#;(begin
    (define cycle-ex-1 (sketch-template
                        #:atoms C-1 C-2 C-3 C-4 C-5
                        #:bonds
                        (C-1 C-2)
                        (C-2 C-3)
                        (C-3 C-4)
                        (C-4 C-5)
                        (C-5 C-1)))
    ; expands to
    (template+
     (list
      (an-element-symbol->template (get-an-element-symbol C) #:id 1)
      (an-element-symbol->template (get-an-element-symbol C) #:id 2)
      (an-element-symbol->template (get-an-element-symbol C) #:id 3)
      (an-element-symbol->template (get-an-element-symbol C) #:id 4)
      (an-element-symbol->template (get-an-element-symbol C) #:id 5))
     (list
      (bond 1 2)
      (bond 2 3)
      (bond 3 4)
      (bond 4 5)
      (bond 5 1)))
    (define cycle-ex-2 (ring 5)) ; should we just implement this right into runtime functions?
    (define template-extension-ex-1
      (extend-template
       cycle-ex-1
       #:atoms (C-6 #f #f #f)
       #:bonds (C-1 C-6) (C-4 C-6))) ; this shouldn't use the same compile time checks as sketch template,
    ; but it should compile similarly,
    ; should expand to
    (template+
     (list
      cycle-ex-1
      (an-element-symbol->template (get-an-element-symbol C) #:id 6))
     (list (bond 1 6))
     (list (bond 4 6))))

;-------------------------------------------------------------------------------
; Template Extension
;-------------------------------------------------------------------------------
(define-syntax (extend-template stx)
  (syntax-parse stx
    ((_ (_ #:atoms a1 ... #:bonds b1 ...) #:atoms a2 ... #:bonds b2 ...)
     #'(sketch-template
        #:atoms a1 ... a2 ...
        #:bonds b1 ... b2 ...))))
;-------------------------------------------------------------------------------
#|
Compiler Pathway
1. check well-formedness of identifiers
2. check well-formedness of complex atoms and bonds
3. check that the graph of all atoms and bonds is connected
4. compile to a template
|#
;-------------------------------------------------------------------------------
(define-syntax (compile-sketch-template->template stx)
  (syntax-parse stx
    ((_ #:atoms a ...
        #:bonds b ...)
     (for-each check-simple-or-complex-atom-id! (syntax->list #'(a ...)))
     (for-each check-simple-or-complex-bond-id! (syntax->list #'(b ...)))
     (for-each check-atom! (syntax->list #'(a ...)))
     (for-each check-bond! (syntax->list #'(b ...)))
     (check-connected! (list (syntax->list #'(a ...))
                             (syntax->list #'(b ...))))
     #`(templates+ (list (compile-atom a) ...)
                   (list (compile-bond b) ...)))))
; ------------------------------------------------------------------------------
; 1. Check well-formedness of identifiers                                      |
; ------------------------------------------------------------------------------
(define-for-syntax (check-simple-or-complex-bond-id! stx)
  (syntax-parse stx
    ((a1 a2 _ _) (for-each check-simple-atom-id! (syntax->list #'(a1 a2))))
    ((a1 a2) (for-each check-simple-atom-id! (syntax->list #'(a1 a2))))))

(define-for-syntax (check-simple-or-complex-atom-id! stx)
  (syntax-parse stx
    ((a:id _ _ _) (check-simple-atom-id! #'a))
    (a (check-simple-atom-id! #'a))))

(define-for-syntax (check-simple-atom-id! stx)
  (define (yell s)
    (raise-syntax-error
     'check-atom-id!
     s
     stx))
  (unless (identifier? stx) (yell "expected an identifier"))
  (define l
    (string-split
     (symbol->string
      (syntax-parse stx
        (x:id (syntax->datum stx))))
     "-"))
  (unless (= (length l) 2)
    (yell (string-append "not a well formed atom-id."
                         "\nexpected <<valid-element-symbol-value?>-<positive-integer?>>"
                         "\na correct example would be Au-14")))
  (match-define (list string-sym string-num) l)
  (define res-sym (string->symbol string-sym))
  (define res-aes (an-element-symbol res-sym))
  (check-aes! res-aes stx)
  (define res-num (string->number string-num))
  (unless (positive-integer? res-num)
    (yell "expected a positive-integer? on the rhs")))

; ------------------------------------------------------------------------------
; 2. Checking well-formedness of atoms and bonds                               |
; ------------------------------------------------------------------------------

(define-for-syntax (check-atom! stx)
  (syntax-parse stx
    ((_ mass-number chirality formal-charge)
     (enforce-contract positive-integer? #'mass-number 'check-atom-mass-number! "a positive-integer")
     (enforce-contract (or/c 'R 'S) #'chirality 'check-atom-chirality! "'R, 'S,")
     (enforce-contract integer? #'formal-charge 'check-atom-formal-charge! "an integer")
     #'(void))
    (_ #'(void))))

(define-for-syntax (check-bond! stx)
  (syntax-parse stx
    ((_ _ order stereo)
     (enforce-contract (or/c 1 2 3) #'order 'check-bond-order! "1, 2, 3,")
     (enforce-contract (or/c 'E 'Z) #'stereo 'check-bond-stereo! "'E, 'Z,")
     #'(void))
    ((_ _) #'(void))))

(define-for-syntax (host-expr->datum stx)
  (cadr (syntax->datum stx)))

(define-for-syntax (enforce-contract a-contract stx caller msg)
  (unless ((or/c a-contract (or/c symbol? #f)) (host-expr->datum stx))
    (raise-syntax-error
     caller
     (string-append "expected: " msg " or #f")
     stx)))
; ------------------------------------------------------------------------------
; 3. Checking the graph of atoms and bonds is connected                        |
; ------------------------------------------------------------------------------
(define-for-syntax (check-connected! l)
  (match-define (list stx-vs stx-es) l)
  (define vs (map compile-atom->vertex stx-vs))
  (define es (map compile-bond->edge stx-es))
  (unless (connected? vs es)
    (raise-syntax-error
     'check-connected!
     "not connected through bonds to all other atoms"
     (car stx-vs))))

(define-for-syntax (connected? vs es)
  (define forest (make-immutable-hash (map (λ (v) (cons v (uf-new v))) vs)))
  (for-each (λ (e) (uf-union! (hash-ref forest (car e)) (hash-ref forest (cdr e)))) es)
  (define sets (hash-values forest))
  (andmap (λ (s) (uf-same-set? s (car sets))) (cdr sets)))

(define-for-syntax (compile-atom->vertex stx)
  (syntax-parse stx
    (x:id (syntax->datum #'x))
    ((x:id _ _ _) (syntax->datum #'x))))

(define-for-syntax (compile-bond->edge stx)
  (syntax-parse stx
    ((x1:id x2:id) (cons (syntax->datum #'x1) (syntax->datum #'x2)))
    ((x1:id x2:id _ _) (cons (syntax->datum #'x1) (syntax->datum #'x2)))))

(module+ test
  (begin-for-syntax
    (check-true (connected? '(a b c d) '((a . b) (b . c) (c . d))))
    (check-false (connected? '(a b c d) '((a . b) (b . c))))))
; ------------------------------------------------------------------------------
; 4. Compiling atoms and bonds to a runtime template                           |
; ------------------------------------------------------------------------------
(define-syntax (compile-atom stx)
  (syntax-parse stx
    ((_ (a:id mass-number chirality formal-charge))
     (match-define (cons aes num-id) (help-compile-atom-id #'a))
     #`(an-element-symbol->template
        (an-element-symbol '#,aes)
        #:id #,num-id
        #:mass-number mass-number
        #:chirality chirality
        #:formal-charge formal-charge)
     )
    ((_ a:id)
     #'(compile-atom (a #f #f #f)))))

(define-syntax (compile-bond stx)
  (syntax-parse stx
    ((_ (a1:id a2:id order stereo))
     (match-define (cons _ num-id1) (help-compile-atom-id #'a1))
     (match-define (cons _ num-id2)(help-compile-atom-id #'a2))
     #`(bond #,num-id1 #,num-id2 order stereo))
    ((_ (a1:id a2:id))
     #'(compile-bond (a1 a2 1 #f)))))

(define-for-syntax (help-compile-atom-id stx)
  (define l
    (string-split
     (symbol->string
      (syntax-parse stx
        (x:id (syntax->datum stx))))
     "-"))
  (match-define (list string-sym string-num) l)
  (define res-sym (string->symbol string-sym))
  (define res-aes (an-element-symbol res-sym))
  (define res-num (string->number string-num))
  (cons res-sym res-num))
