#lang racket

(require "core.rkt")
(require syntax/parse/define)
(require syntax/parse)

; TODO: consider using syntax-parse


; no define form
#;(define-syntax molecule
    (syntax-rules (atoms bonds)
      ;; atoms: [id element], bonds: [id1 id2 order]
      [(_ name (atoms [id element] ...) (bonds [id1 id2 ord] ...))
       (let* ([m (mol (make-immutable-hash) empty)]
              [m (set-atom m 'id (atom 'element #f #f #f))] ...
              [m (add-bond m (bond 'id1 'id2 ord #f))] ...)
         m)]))

(define-syntax molecule
  (syntax-rules (atoms bonds)
    ;; atoms: [id element], bonds: [id1 id2 order]
    [(_ name
        (atoms [id element] ...)
        (bonds [id1 id2 order] ...))
     (define name
       (let* ([m (mol (make-immutable-hash) empty)]
              [m (set-atom m 'id (atom 'element #f #f #f))] ...
              [m (add-bond m (bond 'id1 'id2 order #f))] ...)
         m))]))


(module+ test
  (require rackunit)
  (molecule water
            (atoms [O O] [H1 H] [H2 H])
            (bonds [O H1 1] [O H2 1]))
  (check-equal? water (mol (hash 'H1 (atom 'H #f #f #f)
                                 'H2 (atom 'H #f #f #f)
                                 'O (atom 'O #f #f #f))
                           (list (bond 'O 'H2 1 #f) (bond 'O 'H1 1 #f)))))
