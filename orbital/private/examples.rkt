#lang racket
(require "templates-and-substituents.rkt"
         "templates-and-substituents-stx.rkt"
         "types.rkt"
         "render2d.rkt"
         "mol-to-cml.rkt"
         "babel.rkt"
         "periodic-table.rkt"
         "render3d.rkt"
         "cid.rkt"
         "explore.rkt"
         (for-syntax syntax/parse))

(define (mol->pict m)
  (png->pict (babel (mol->cml m) png)))

(define (mol->pict3d m)
  ((compose mol3d->pict3d cid->mol3d smiles->cid)
   (babel (mol->cml m) smiles)))

(define (explore-mol m)
  (explore (mol->pict3d m)))

(define-for-syntax (ct:element-id->template stx)
  (syntax-parse stx
    ((id a1 ...)
     #'(an-element-symbol->template
      (get-an-element-symbol id) a1 ...))))

(define-syntax (element-id->template stx)
  (syntax-parse stx
    ((_ id a1 ...)
     (ct:element-id->template #'(id a1 ...)))))



(define-syntax (element-id->substituent stx)
  (syntax-parse stx
    ((_ id a1 ...)
     #`(template->substituent
        #,(ct:element-id->template #'(id a1 ...))))))


(define-syntax (element-id->hydrogenated-substituent stx)
  (syntax-parse stx
    ((_ id h-count)
     #`(template->hydrogenated-substituent
        #,(ct:element-id->template #'(id))
        h-count))))

(define -H (element-id->substituent H))

(define (template->hydrogenated-substituent t count #:id [id 1])
  (template->substituent
   (add-substituent t
                    (info-substituent-addition
                     -H
                     (bond id 1 1 #f)
                     count
                     #f)
                    )))

#|
Functional Groups:

[Carbon Chains]
-alkyl chain
-iPr
-tBu
-carbon ring
-aromatic ring

[Hetero Atoms]
Halides
-OH
-NH2
|#

(define (alkyl-chain n)
  ; a carbon chain of length 3:
  ; CH2 - CH2 - CH3
  (define CH2 (element-id->hydrogenated-substituent C 2))
  (define -CH3 (element-id->hydrogenated-substituent C 3))
  (let loop ((num-CH2-left (sub1 n))
             (acc -CH3))
    (if (zero? num-CH2-left)
        acc
        (loop (sub1 num-CH2-left)
              (template->substituent
               (add-substituent CH2
                                (info-substituent-addition
                                 acc
                                 (bond 1 1 1 #f)
                                 1
                                 #f)))))))

; uses implicit hydrogens!
(define (alkyl-ring n)
  (define ts
    (build-list
     n
     (λ (n)
       (element-id->template C #:id (add1 n)))))
  (define bs
    (cons (bond n 1 1 #f)
          (build-list (sub1 n) (λ (n) (bond (add1 n) (+ n 2) 1 #f)))))
  (match-define (template atoms bonds nid baids) (templates+ ts bs))
  (template->substituent (template atoms bonds nid (list 1))))

(define -iPr
  (template->hydrogenated-substituent
   (add-substituents (element-id->template C)
                     (info-substituent-addition
                      (alkyl-chain 1)
                      (bond 1 1 1 #f)
                      2
                      #f))
   1))

(define -tBu
  (template->substituent
   (add-substituents (element-id->template C)
                     (info-substituent-addition
                      (alkyl-chain 1)
                      (bond 1 1 1 #f)
                      3
                      #f))))

(define phenyl-template
  (sketch-template
   #:atoms
   C-1 C-2 C-3 C-4 C-5 C-6
   #:bonds
   (C-1 C-2 2 #f)
   (C-2 C-3)
   (C-3 C-4 2 #f)
   (C-4 C-5)
   (C-5 C-6 2 #f)
   (C-6 C-1)))

(define ex-1-phenyl-with-cyclopropyls
  (add-substituents phenyl-template
                    (info-substituent-addition
                      (alkyl-ring 3)
                      (bond 1 1 1 #f)
                      1
                      #f)
                    (info-substituent-addition
                      (alkyl-ring 3)
                      (bond 2 1 1 #f)
                      1
                      #f)
                    (info-substituent-addition
                      (alkyl-ring 3)
                      (bond 3 1 1 #f)
                      1
                      #f)
                    (info-substituent-addition
                      (alkyl-ring 3)
                      (bond 4 1 1 #f)
                      1
                      #f)
                    (info-substituent-addition
                      (alkyl-ring 3)
                      (bond 5 1 1 #f)
                      1
                      #f)
                    (info-substituent-addition
                      (alkyl-ring 3)
                      (bond 6 1 1 #f)
                      1
                      #f)))

; we can reduce duplication by using the bonding-ids
; and list abstractions
(define (add-substituent-to-each-bonding-atom t isa)
  (match-define
    (info-substituent-addition subst
                               (bond _ sid order stereo)
                               num-times
                               _)
    isa)
  (match-define (template as bs _ baids) t)
  (apply add-substituents t
         (map (λ (baid) (info-substituent-addition
                         subst (bond baid sid order stereo)
                         num-times #f))
              baids)))

(define ex-2-phenyl-with-cyclopropyls
  (add-substituent-to-each-bonding-atom phenyl-template
                                        (info-substituent-addition
                                         (alkyl-ring 3)
                                         (bond 1 1 1 #f)
                                         1
                                         #f)))

         