#lang racket
(require net/http-client)
(require net/url)
(require json)
(require file/sha1)
(require racket/draw)
(require rackunit)

(provide
 element ; struct to represent an element on the periodic table
 dirty-elements ; pure json from pubchem, a list of elements
 clean-elements) ; cleaner racket data, a list of elements

; goal: racket periodic table

#|
[extracted-json] : the json from the periodic table from PubChem
|#
(define PT-URL (string->url "https://pubchem.ncbi.nlm.nih.gov/rest/pug/periodictable/JSON"))

(define conn-to-pubchem (http-conn-open (url-host PT-URL) #:ssl? #t))

(match-define-values (a b in) (http-conn-sendrecv! conn-to-pubchem  (url->string PT-URL))) ; input is a gzip

(define extracted-json (read-json in))

(http-conn-close! conn-to-pubchem)

#|
Data Defintion: An element is a structure with all the fields from the columns specified in the extracted JSON
The predicates shall evaluate true for fields of cleaned elements.
|#

(struct element
  (atomic-number ; number?
   symbol ; symbol?
   name ; symbol? 
   atomic-mass ; number?
   cpk-color ; (or/c number? #f)
   electron-configuration ; string?
   electronegativity ; number?
   atomic-radius ; number?
   ionization-energy ; number?
   electron-affinity ; number?
   oxidation-states ; (listof number?)
   standard-state ; symbol?
   melting-point ; number?
   boiling-point ; number?
   density ; number?
   group-block ; string?
   year-discovered ; (or/c number? #f)
   ) #:prefab)

(define dirty-elements (map (λ (x) (apply element (hash-ref x 'Cell))) (hash-ref (hash-ref extracted-json 'Table) 'Row)))

(define (color-mapper hex)
  (let ((maybe-three-bytes (bytes->list (hex-string->bytes hex))))
    (if (and (list? maybe-three-bytes) (= (length maybe-three-bytes) 3))
        (apply make-object color% maybe-three-bytes)
        #f)))

(module+ test
  (check-equal? (electron-configuration-mapper "1s1") '((1 s 1)))
  (check-equal? (electron-configuration-mapper "[He]2s2 2p1") '(He (2 s 2) (2 p 1)))
  (check-equal? (electron-configuration-mapper "[He] 2s2 2p1") '(He (2 s 2) (2 p 1))))

(define (electron-configuration-mapper ec)
  (let* ((ec (string-split ec))
        
         (ec (foldr
              (λ (x acc) (if (equal? (substring x 0 1) "[")

                             ;     012
                             ; [He]2s2
                             ; 01234
                             (list* (substring x 1 3) (substring x 4) acc)
                             (list* x acc)))
              '()
              ec))
         (ec (filter-map
              (λ (x) (cond
                       [(equal? x "") #f]
                       [(string->number (substring x 0 1)) (list (string->number (substring x 0 1))
                                                                 (string->symbol (substring x 1 2))
                                                                 (string->number (substring x 2)))]
                       [(equal? (substring x 0 1) "(") #f]
                       [else (string->symbol x)]))
             ec)))
    ec))
  
(define (oxidation-states-mapper os)
  (cond [(equal? "" os) '()]
        [else (map (compose string->number string-trim) (string-split os ","))]))

(define (standard-state-mapper x)
  (cond [(string-contains? x "olid") 'solid]
        [(string-contains? x "iquid") 'liquid]
        [(string-contains? x "as") 'gas]
        [else #f]))

(define (clean-element e)
  (element
   (string->number (element-atomic-number e))
   (string->symbol (element-symbol e))
   (string->symbol (element-name e))
   (string->number (element-atomic-mass e)) 
   (color-mapper (element-cpk-color e))
   (electron-configuration-mapper (element-electron-configuration e))
   (string->number (element-electronegativity e))
   (string->number (element-atomic-radius e))
   (string->number (element-ionization-energy e)) 
   (string->number (element-electron-affinity e)) 
   (oxidation-states-mapper (element-oxidation-states e)) 
   (standard-state-mapper (element-standard-state e))
   (string->number (element-melting-point e))
   (string->number (element-boiling-point e)) 
   (string->number (element-density e)) 
   (element-group-block e) 
   (string->number (element-year-discovered e)) 
   ))

(define clean-elements (map clean-element dirty-elements))
  
   
                                  


