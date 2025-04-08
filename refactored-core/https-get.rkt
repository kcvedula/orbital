#lang racket

(provide/contract
 (https-get (-> string? (-> input-port? any/c) any/c)))

(require net/http-client
         net/url
         "types.rkt")



(define (https-get s handle-in)
  (define u (string->url s))
  (define conn-to-pubchem (http-conn-open (url-host u) #:ssl? #t))

  (match-define-values (status headers in) (http-conn-sendrecv! conn-to-pubchem  (url->string u)))

  (define raw (handle-in in))

  (http-conn-close! conn-to-pubchem)
  (https-get-resp status headers raw))

