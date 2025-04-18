#lang racket

(provide (contract-out (png->pict (-> png? pict?))))

(require (only-in pict bitmap pict?)
         racket/draw
         "types.rkt")

(define (png->pict p)
  (define img
    (make-object bitmap%
      (open-input-bytes (png-v p))))
  (bitmap img))
