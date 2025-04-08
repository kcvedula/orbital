#lang racket

(provide periodic-table)

(require "types.rkt"
         "load-periodic-table.rkt")

(define path "data-periodic-table.rktd")

(define (get-periodic-table)
  (call-with-input-file path read))

(define periodic-table
  (cond [(file-exists? path)
         (get-periodic-table)]
        [else (begin
                (load-periodic-table path)
                (get-periodic-table))]))
  