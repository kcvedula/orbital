#lang typed/racket
(require pict3d
         "explore.rkt"
         "utils.rkt")
(provide CARBON BOND sym+ bond carbon)
(define CARBON (sphere origin 1))
(define BOND (cylinder origin (dir .125 .125 2)))

(: yaw->dir (-> Real Dir))
(define (yaw->dir y)
  (angles->dir y 0))

(: surface/normal/unsafe (->* (Pict3D Dir) (#:inside? Any) (Values Pos Dir)))
(define (surface/normal/unsafe pict dv #:inside? [inside? #f])
  (define-values (pos dir) (surface/normal pict dv #:inside? inside?))
  (values (cast pos Pos) (cast dir Dir)))

(: carbon (-> Sym/Int Pict3D))
(define (carbon name)
  (combine
   CARBON
   (basis (sym+ name 'front) (call-with-values (thunk (surface/normal/unsafe CARBON (yaw->dir 0))) point-at))
   (basis (sym+ name 'left) (call-with-values (thunk (surface/normal/unsafe CARBON (yaw->dir 120))) point-at))
   (basis (sym+ name 'right) (call-with-values (thunk (surface/normal/unsafe CARBON (yaw->dir 240))) point-at))))

(: bond (-> Sym/Int Sym/Int Pict3D))
(define (bond c1 c2)
  (combine
   BOND
   (basis (sym+ c2 c1) (point-at (pos 0 0 1) -z))
   (basis (sym+ c1 c2) (point-at (pos 0 0 -1) +z))))

#;'((define ACC-1 (join C1 '(c1front) B1 '(b1c1)))
    (define ACC-2 (join C2 '(c2front) ACC-1 '(b1c2)))
    (define ACC-3 (join B2 '(b2c2) ACC-2 '(c2left)))
    (define ACC-4 (join C3 '(c3front) ACC-3 '(b2c3)))
    (define ACC-5 (join B3 '(b3c2) ACC-4 '(c2right)))
    (define ACC-6 (join C4 '(c4front) ACC-5 '(b3c4))))

#;(explore ACC-6)

; we want to abstract this to just give a range and it should work
; and we want to be able to nest these easily

(define C1  (carbon 'c1))
(define C2 (carbon 'c2))
(define B1 (bond 'c1 'c2))
(define TL-ACC-1 (join C1 '(c1front) B1 '(c2c1)))
(define TL-ACC-2 (join C2 '(c2front) TL-ACC-1 '(c1c2)))

(: sp2/2 (-> Sym/Int Sym/Int Sym/Int Pict3D Pict3D))
(define (sp2/2 root cl cr pict)
  (define CLN (sym+ 'c cl))
  (define CRN (sym+ 'c cr))
  (define RN (sym+ 'c root))
  (define CL (carbon CLN))
  (define CR (carbon CRN))
  (define BL (bond RN CLN))
  (define BR (bond RN CRN))
  (define ACC-1 (join pict (list (sym+ RN 'left)) BL (list (sym+ RN CLN))))
  (define ACC-2 (join CL (list (sym+ CLN 'front)) ACC-1 (list (sym+ CLN RN))))
  (define ACC-3 (join BR (list (sym+ RN CRN)) ACC-2 (list (sym+ RN 'right))))
  (define ACC-4 (join CR (list (sym+ CRN 'front)) ACC-3  (list (sym+ CRN RN))))
  ACC-4)
(define TL-ACC-3 (sp2/2 2 3 4 TL-ACC-2))
(define TL-ACC-4 (sp2/2 1 5 6 TL-ACC-3))
(define TL-ACC-5 (sp2/2 3 7 8 TL-ACC-4))
(define TL-ACC-6 (sp2/2 4 9 10 TL-ACC-5))
(define TL-ACC-7 (sp2/2 9 11 12 TL-ACC-6))

(explore TL-ACC-7)
