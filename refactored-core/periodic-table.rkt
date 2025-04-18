#lang racket

(provide
 (rename-out
  (clean-elements periodic-table)))

(require net/http-client
         net/url
         json
         file/sha1
         racket/draw
         racket/runtime-path
         racket/struct
         rackunit
         "https-get.rkt"
         (only-in "types.rkt"
                  https-get-resp-raw
                  an-element-symbol
                  (element clean-element)))

(define-runtime-path saved-periodic-table-path "periodic-table.rktd")

(define (extract-periodic-table-json-from-internet)
  (define PT-URL-STRING "https://pubchem.ncbi.nlm.nih.gov/rest/pug/periodictable/JSON")
  (https-get-resp-raw (https-get PT-URL-STRING read-json))
  (with-output-to-file saved-periodic-table-path
    (thunk (write (https-get-resp-raw (https-get PT-URL-STRING read-json))))
    #:exists 'replace))

(define (periodic-table-json-saved?)
  (file-exists? saved-periodic-table-path))

(define (read-saved-periodic-table-json)
  (call-with-input-file saved-periodic-table-path read))

(define extracted-json (if (periodic-table-json-saved?)
                           (read-saved-periodic-table-json)
                           (begin (extract-periodic-table-json-from-internet)
                                  (read-saved-periodic-table-json))))

(struct raw-element
  [atomic-number ; number?
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
   year-discovered] ; (or/c number? #f)
  #:prefab)

(define dirty-elements
  (map
   (λ (x)
     (apply raw-element (hash-ref x 'Cell)))
   (hash-ref (hash-ref extracted-json 'Table) 'Row)))

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
              (λ (x acc)
                (if (equal? (substring x 0 1) "[")
                    ;     012
                    ; [He]2s2
                    ; 01234
                    (list* (substring x 1 3) (substring x 4) acc)
                    (list* x acc)))
              '()
              ec))
         (ec (filter-map
              (λ (x)
                (cond
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

(define (clean-raw-element e)
  (raw-element
   (string->number (raw-element-atomic-number e))
   ((compose an-element-symbol string->symbol) (raw-element-symbol e))
   (string->symbol (raw-element-name e))
   (string->number (raw-element-atomic-mass e))
   (color-mapper (raw-element-cpk-color e))
   (electron-configuration-mapper (raw-element-electron-configuration e))
   (string->number (raw-element-electronegativity e))
   (string->number (raw-element-atomic-radius e))
   (string->number (raw-element-ionization-energy e))
   (string->number (raw-element-electron-affinity e))
   (oxidation-states-mapper (raw-element-oxidation-states e))
   (standard-state-mapper (raw-element-standard-state e))
   (string->number (raw-element-melting-point e))
   (string->number (raw-element-boiling-point e))
   (string->number (raw-element-density e))
   (raw-element-group-block e)
   (string->number (raw-element-year-discovered e))))

(define (element->clean-element e)
  (apply clean-element (struct->list e)))

(define clean-elements
  (map
   (compose element->clean-element clean-raw-element)
   dirty-elements))
