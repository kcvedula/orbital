#lang racket
(require net/http-client)
(require net/url)
(require json)
(require file/sha1)
(require racket/draw)

; goal: racket periodic table

(define PT-URL (string->url "https://pubchem.ncbi.nlm.nih.gov/rest/pug/periodictable/JSON"))

(define conn-to-pubchem (http-conn-open (url-host PT-URL) #:ssl? #t))

(match-define-values (a b in) (http-conn-sendrecv! conn-to-pubchem  (url->string PT-URL))) ; input is a gzip

(define extracted-json (read-json in))

(struct atom
  (atomic-number ; string->number
   symbol ; string->symbol
   name ; string->symbol 
   atomic-mass ; string->number 
   cpk-color ; (compose bytes->list hex-string->bytes)
   electron-configuration ; skip for now
   electronegativity ; string->number
   atomic-radius ; string->number 
   ionization-energy ; string->number
   electron-affinity ; string-> number
   oxidation-states ; skip for now
   standard-state ; string->symbol
   melting-point ; string->number
   boiling-point ; string->number
   density ; string->number
   group-block ; string->symbol
   year-discovered ; string->number
   ) #:prefab)


(define atoms1 (hash-ref (hash-ref extracted-json 'Table) 'Row))
(define atoms2 (map (λ (x) (apply atom (hash-ref x 'Cell))) atoms1))
(define (cleansing-pass a)
  (atom
   (string->number (atom-atomic-number a))
   (string->symbol (atom-symbol a))
   (string->symbol (atom-name a))
   (string->number (atom-atomic-mass a)) 
   ((compose  (λ (x)
                (if (and (list? x) (= (length x) 3))
                         (make-object color% (first x) (second x) (third x))
                         #f))
                bytes->list hex-string->bytes) (atom-cpk-color a)) ; (compose bytes->list hex-string->bytes)
   (atom-electron-configuration a) ; skip for now
   (string->number (atom-electronegativity a))
   (string->number (atom-atomic-radius a)) ; string->number 
   (string->number (atom-ionization-energy a)) ; string->number
   (string->number (atom-electron-affinity a)) ; string-> number
   (atom-oxidation-states a) ; skip for now
   (string->symbol (atom-standard-state a)) ; string->symbol
   (string->number (atom-melting-point a)) ; string->number
   (string->number (atom-boiling-point a)) ; string->number
   (string->number (atom-density a)) ; string->number
   (atom-group-block a) ; nothing
   (string->number (atom-year-discovered a)) ; string->number
   ))

(define atoms3 (map cleansing-pass atoms2))
  
   
                                  


