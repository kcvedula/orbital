#lang racket

(provide (all-defined-out))

(require "types.rkt"
         racket/struct
         racket/hash)

; [Constructors]

(define (an-element-symbol->template aes
                                     #:id [id 1]
                                     #:mass-number [mass-number #f]
                                     #:chirality [chirality #f]
                                     #:formal-charge [formal-charge #f])
  (template
   (make-immutable-hash (list (cons id (atom aes mass-number chirality formal-charge))))
   empty
   (add1 id)
   (list id)))


; [Set/Delete]

; check they actually exist in there
(define (remove-bonding-atom-ids t . baids-to-remove)
  (match-define (template atoms bonds nid all-baids) t)
  (template atoms bonds nid (set-subtract all-baids baids-to-remove)))

(define (alpha-offset t offset-amount)
  (match-define (template atoms1 bonds1 nid1 baids1) t)
  (define inc (λ (x) (+ x offset-amount)))
  (define nid2 (inc nid1))
  (define baids2 (map inc baids1))
  (define (bond-mapper b)
    (match-define (bond a1 a2 order stereo) b)
    (bond (inc a1) (inc a2) order stereo))
  (define bonds2 (map bond-mapper bonds1))
  (define (id-atom-mapper id atom)
    (cons (inc id) atom))
  (define atoms2 (make-immutable-hash (hash-map atoms1 id-atom-mapper )))
  (template atoms2 bonds2 nid2 baids2))

; [Core Operations]

; check template bonding ids
(define (add-substituents t . isas)
  (define (folder isa acc)
    (add-substituent acc isa))
  (foldl folder isas))

; assumes the bond in isa has the template bonding atom id first
; and then the substituent bonding id
; check template bonding ids
(define (add-substituent t isa)
  (match-define (info-substituent-addition s1 b num-times) isa)
  (match-define (bond tbaid sbaid1 order stereo) b)
  (match-define (template _ _ t-nid1 _) t)
  (match-define (substituent _ _ s-nid1 _) s1)
  (define t-nid2 (+ t-nid1 (sub1 s-nid1)))
  (define s2 (alpha-offset
               (remove-bonding-atom-ids s1 sbaid1) (sub1 t-nid1))) ; increment substituent
  (define sbaid2 (+ sbaid1 (sub1 t-nid1)))
  (define res (template+ t s2 (bond tbaid sbaid2 order stereo)))
  (if (= 1 num-times)
      res
      (add-substituent res (info-substituent-addition
                            s1 b (sub1 num-times)))))

; check overlapping ids and check that the bonds only reference
; bonding atom ids
#;(define (template+ t1 t2 . bs)
  (match-define (template atoms1 bonds1 nid1 baids1) t1)
  (match-define (template atoms2 bonds2 nid2 baids2) t2)
  (template (hash-union atoms1 atoms2)
            (append bs bonds1 bonds2)
            (max nid1 nid2)
            (append baids1 baids2)))

(define (templates+ ts bs)
  (template
   (apply hash-union (map mol-atoms ts))
   (apply append bs (map mol-bonds ts))
   (apply max (map template-next-id ts))
   (apply append (map template-bonding-atoms-ids ts))))

(define (template+ t1 t2 . bs)
  (templates+ (list t1 t2) bs))
   

; [Conversion]

(define (template->substituent-possible? t)
  (match-define (template atoms bonds nid ids) t)
  (= (length ids) 1))

(define (template->substituent t)
  (unless (template->substituent-possible? t)
    (error 'template->substituent "too many bonding-atoms to convert template to substituent"))
  (apply substituent (struct->list t)))
