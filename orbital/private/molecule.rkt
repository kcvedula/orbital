#lang racket

(provide (all-defined-out))

(define obabel-command (make-parameter #f))

(define (obabel s)
  (unless (obabel-command) (error 'obabel-command "not initialized"))
  (match-define (list out in pid err proc)
    (process (string-append (obabel-command) " " s)))
  (proc 'wait)
  (define res (port->string out))
  ; cleanup
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  res
  )

(define (obabel/smiles input-smiles input-other)
  (string-trim (obabel (string-append "-:" "\"" input-smiles "\"" " " input-other))))

(define (obabel/canonical-smiles input-smiles)
  (obabel/smiles input-smiles "-ocan"))

(define (valid-smiles? s)
  (not (equal? (obabel/canonical-smiles s) "")))

(define (smiles->canonical-smiles s)
  (define res (obabel/canonical-smiles s))
  (if (equal? res "")
      (error 'smiles->canonical-smiles "expected valid smiles")
      res))

(obabel-command "wsl obabel")



  
  








