#lang racket
(require "mol.rkt"
         "types.rkt"
         "render2d.rkt" ; testing
         "babel.rkt" ; testing
         "periodic-table.rkt"
         pict/convert ; testing
         racket/struct
         racket/hash)

(begin-for-syntax
  (require "periodic-table.rkt")
  (require "types.rkt")
  (require syntax/parse)
  
  (define (valid-symbol? s)
    (pair? (member (an-element-symbol s) (map element-symbol periodic-table))))
  )

; C => (an-element-symbol 'C)
; foo => error
; 
(define-syntax (get-an-element-symbol stx)
  (syntax-parse stx
    ((_ s)
     (if (valid-symbol? (syntax->datum #'s))
         #'(an-element-symbol (syntax->datum #'s))
         (raise-syntax-error
          'get-an-element-symbol
          "not a valid symbol in the periodic table"
          #'s))
    )))

(define (an-element-symbol->atom x)
  (if (member x (map element-symbol periodic-table))
      (atom x #f #f #f)
      (error 'an-element-sybol
             "(an-element-symbol ~v) is not in the periodic table"
             x)))

(define atom-H (an-element-symbol->atom (get-an-element-symbol H)))

(define atom-C (an-element-symbol->atom (get-an-element-symbol C)))

(struct template (mol next-id bonding-atoms-ids) #:transparent)

(struct substituent template ()
  #:transparent
  ;#:property prop:pict-convertible (λ (x) (substituent->pict x))
  )

; [Constructors]

(define (atom->template a #:id [id 1])
  (template
   (mol (make-immutable-hash (list (cons id a))) '())
   (add1 id)
   (list id)))

(define (an-element-symbol->template aes #:id [id 1])
  (atom->template (an-element-symbol->atom aes) id))

; [Add/Delete]

(define (remove-template-bonding-atom-ids t . baids-to-remove)
  (match-define (template m nid all-baids) t)
  (template m nid (set-subtract all-baids baids-to-remove)))

; [Composition]

(define (add-substituents t subs-and-bonds-and-num-times)
  (define (handle-sub-and-bond-and-num-time/acc sbnt acc)
    (match-define (list s b nt) sbnt)
    (add-substituent acc s b nt))
  (foldl handle-sub-and-bond-and-num-time/acc t subs-and-bonds-and-num-times))

(define (add-substituent t s1 b [num-times 1])
  (match-define (bond tbaid sbaid1 order stereo) b)
  (match-define (template _ t-nid1 _) t)
  (match-define (substituent _ s-nid1 _) s1) ; assume that nid2 - 1 = bonding-id
  ; say the template has baids 1, 2, 3, 4, nid = 5
  ; and the substituent has baids 1, 2, 3, 4, 5, nid = 6
  ; we want the template's new nid to be 10, which is (+ nid1 (sub1 nid2))
  ; we need to add 4 to every single substituent id to prevent collision
  (define t-nid2 (+ t-nid1 (sub1 s-nid1)))
  (define s2 (increment-template
               (remove-template-bonding-atom-ids s1 sbaid1) (sub1 t-nid1))) ; increment substituent
  (define sbaid2 (+ sbaid1 (sub1 t-nid1)))
  (define res (template+ t s2 (bond tbaid sbaid2 order stereo)))
  (if (= 1 num-times)
      res
      (add-substituent res s1 b (sub1 num-times))))
  
(define (template+ t1 t2 . bs)
  (match-define (template (mol atoms1 bonds1) nid1 baids1) t1)
  (match-define (template (mol atoms2 bonds2) nid2 baids2) t2)
  (unless (set-empty? (set-intersect (hash-keys atoms1) (hash-keys atoms2)))
    (error 'template+ "overlapping ids"))
  (template (mol (hash-union atoms1 atoms2) (append bs bonds1 bonds2))
                 (max nid1 nid2)
                 (append baids1 baids2)))
  
(define (increment-template t increment-amount)
  (match-define (template (mol atoms1 bonds1) nid1 baids1) t)
  (define inc (λ (x) (+ x increment-amount)))
  (define nid2 (inc nid1))
  (define baids2 (map inc baids1))
  (define (bond-mapper b)
    (match-define (bond a1 a2 order stereo) b)
    (bond (inc a1) (inc a2) order stereo))
  (define bonds2 (map bond-mapper bonds1))
  (define (id-atom-mapper id atom)
    (cons (inc id) atom))
  (define atoms2 (make-immutable-hash (hash-map atoms1 id-atom-mapper )))
  (template (mol atoms2 bonds2) nid2 baids2))

; [Conversion]

(define (template->substituent-possible? t)
  (match-define (template m nid ids) t)
  (= (length ids) 1))

(define (template->substituent t)
  (unless (template->substituent-possible? t)
    (error 'template->substituent "too many bonding-atoms to convert template to substituent"))
  (apply substituent (struct->list t)))

(define (template->mol-possible? t)
  (empty? (template-bonding-atoms-ids t))
  )

(define (template->mol t)
  (unless (template->mol-possible? t)
    (error 'template->mol "cannot convert a template with bonding atoms"))
  (template-mol t))

; *forces hydrogenation
(define (substituent->mol s)
  (match-define (substituent _ _ (list bid)) s)
  (template->mol (remove-template-bonding-atom-ids
                  (add-substituent
                   s
                   (template->substituent (atom->template atom-H))
                   (bond bid 1 1 #f))
                  bid)))

; [Rendering] (TODO: remove after finalization)

(define (smiles->pict s)
  (png->pict (babel s png)))

(define (mol->pict m)
  (png->pict (babel (mol->cml m) png)))

(define template->pict (compose mol->pict template->mol))

(define substituent->pict (compose mol->pict substituent->mol))

(define ex-template
  (atom->template atom-C))

(define ex-substituent
  (template->substituent (atom->template atom-H)))

(define mol-CH4-ex1
  (let* ((m empty-mol)
         (m (set-atom m 1 atom-C))
         (m (set-atom m 2 atom-H))
         (m (set-atom m 3 atom-H))
         (m (set-atom m 4 atom-H))
         (m (set-atom m 5 atom-H))
         (m (add-bond m (bond 1 2 1 #f)))
         (m (add-bond m (bond 1 3 1 #f)))
         (m (add-bond m (bond 1 4 1 #f)))
         (m (add-bond m (bond 1 5 1 #f))))
  m))

(define mol-CH4-ex2
  (template->mol
   (remove-template-bonding-atom-ids
    (add-substituent
     (atom->template atom-C)
     (template->substituent (atom->template atom-H))
     (bond 1 1 1 #f)
     4) 1)))

(define -H (template->substituent (atom->template atom-H)))

(define -CH3
  (template->substituent
   (add-substituent
    (atom->template atom-C)
    -H
    (bond 1 1 1 #f)
    3)))

(define C3H8
  (template->mol
   (remove-template-bonding-atom-ids
    (add-substituents
     (atom->template atom-C)
     (list (list -CH3 (bond 1 1 1 #f) 1)
           (list -H (bond 1 1 1 #f) 1)
           (list -CH3 (bond 1 1 1 #f) 1)
           (list -H (bond 1 1 1 #f) 1)))
    1)))
          