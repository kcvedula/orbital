#lang racket

(provide
 cmd-prefix
 (contract-out
  (babel (-> (or/c smiles? cml?) (or/c smiles cml sdf png) (or/c smiles? cml? sdf? png?)))
  ))

(require "types.rkt")

(define cmd-prefix (make-parameter ""))
(cmd-prefix "wsl")

(define (system/in cmd in [bytes? #f])
  (define pref+cmd (string-append (cmd-prefix) " " cmd))
  (define the-thunk (thunk (system pref+cmd)))
  (define res
    (parameterize
        ((current-error-port (open-output-nowhere))) ; (open-output-nowhere)
      (with-input-from-string in
        (thunk (if bytes?
                   (with-output-to-bytes the-thunk)
                   (with-output-to-string the-thunk))))))
  (if (string? res) (string-trim res) res))

(define (babel in output-callback)
  (define-values
    (input-flag input-data)
    (match in
      ((smiles s) (values " -ismi - " s))
      ((cml s) (values " -icml - " s))))
  
  (define output-flag
    (match output-callback
      ((== smiles) "-ocan")
      ((== cml) "-ocml")
      ((== sdf) "-osdf --gen3d")
      ((== png) "-opng --gen2d")))

  (output-callback
   (system/in
    (string-append "obabel " input-flag output-flag)
    input-data
    (equal? output-callback png))))

