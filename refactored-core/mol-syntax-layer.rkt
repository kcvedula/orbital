#lang racket
(require "templates-and-substituents.rkt"
         "types.rkt"
         "periodic-table.rkt"
         "render2d.rkt" ; testing
         "mol.rkt" ; testing
         "babel.rkt" ; testing
         racket/hash
         racket/struct
         syntax-spec-v3
         syntax/parse
         (for-syntax
          syntax/parse
          syntax-spec-v3
          racket/string
          racket/match
          racket/math
          racket/struct
          rackunit
          "types.rkt"
          "periodic-table.rkt")
         )

; [Rendering, Only for Testing]

(define (mol->pict m)
  (png->pict (babel (mol->cml m) png)))

; [Static Check on Element Symbols]

(define-for-syntax (valid-aes? aes)
  (pair? (member aes (map element-symbol periodic-table))))

(define-for-syntax (check-aes! aes ctxt)
  (unless (valid-aes? aes)
    (raise-syntax-error
     'periodic-table
     "invalid element-symbol"
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



(syntax-spec
 (host-interface/expression
  (sketch-template #:atoms a:spec-atom ... #:bonds b:spec-bond ...)
  #:binding (scope (import a) ...)
  #'(begin
      (compile-sketch-template->template
       #:atoms a ...
       #:bonds b ...)))
 
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
              (a1:spec-atom a2:spec-atom)
              #:binding (scope (import a1) (import a2))
              ; complex
              (a1:spec-atom
               a2:spec-atom
               order:racket-expr
               stereo:racket-expr)
              #:binding (scope (import a1) (import a2))))

(define lazily-written-benzene
  (sketch-template
   #:atoms
   C-1 C-2 C-3 C-4 C-5 C-6
   #:bonds
   (C-1 C-2 2 #f)
   (C-2 C-3)
   (C-3 C-4 2 #f)
   (C-4 C-5)
   (C-5 C-6 2 #f)
   (C-6 C-1)))


(define-syntax (compile-sketch-template->template stx)
  (syntax-parse stx
    ((_ #:atoms a ...
        #:bonds b ...)
     #'(templates+ (compile-atoms a ...)
                   (compile-bonds b ...)))))

; necessary checks:
; all element symbols in lhs of atom identifiers are valid
; atom symbol follows the shape of <id-positive-integer>

; bonds only refer to atoms that are already referenced
; in #:atoms ...
; (I think that syntax-specv3 gives that for free but i'm not
; sure how to do that)

; #:atoms a ... -> templates
; #:bonds b ... -> list of bonds

(define-syntax (compile-atoms stx)
  (syntax-parse stx
    ((_ a ...)
     #'(list (compile-atom a) ...))))

(define-syntax (compile-bonds stx)
  (syntax-parse stx
    ((_ b ...)
     #'(list (compile-bond b) ...))))


(define-syntax (compile-atom stx)
  (syntax-parse stx
    ((_ a:id mass-number chirality formal-charge)
     (match-define (cons aes num-id) (help-compile-atom-id #'a))
     #`(an-element-symbol->template
        (an-element-symbol '#,aes)
        #:id #,num-id
        #:mass-number mass-number
        #:chirality chirality
        #:formal-charge formal-charge)
     )
    ((_ a:id)
     #'(compile-atom a #f #f #f))))

(define-syntax (compile-bond stx)
  (syntax-parse stx
    ((_ (a1:id a2:id order stereo))
     (match-define (cons _ num-id1) (help-compile-atom-id #'a1))
     (match-define (cons _ num-id2)(help-compile-atom-id #'a2))
     #`(bond #,num-id1 #,num-id2 order stereo))
    ((_ (a1:id a2:id))
     #'(compile-bond (a1 a2 1 #f)))))



; an atom id is like C-1
; that it follows <<valid-element-symbol-value?>-<positive-integer?>>
; and is an identifier
(define-for-syntax (help-compile-atom-id stx)
  (define (yell s)
    (raise-syntax-error
     'compile-atom
     s
     stx))
  
  (unless (identifier? stx) (yell "expected identifier"))
  
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
    (yell "expected a positive-integer? on the rhs"))
  (cons res-sym res-num)
  )
