#lang typed/racket
(require pict3d
         pict3d/universe
         "gyroscope.rkt"
         "sp2.rkt"
         "explore.rkt"
         "utils.rkt")
;; the points of a tetrahedron
;; [0 0 0]
;; [1 0 1]
;; [1 1 0]
;; [0 1 1]
;; these are all centered at .5 .5 .5, so subtracting that from everywhere, then multiplying by 2
;; [-1 -1 -1]
;; [1 -1 1]
;; [1 1 -1]
;; [-1 1 1]

(: pos&dir (-> Real Real Real (Values Pos Dir)))
(define (pos&dir x y z)
  (match-define (dir dx dy dz) (dir-normalize (dir x y z)))
  (values (pos dx dy dz) (dir dx dy dz)))

(define (v1) (pos&dir 1 1 1)) ;; p1
(define (v2) (pos&dir -1 -1 1)) ;; p2
(define (v3) (pos&dir 1 -1 -1))
(define (v4) (pos&dir -1 1 -1))

;; bond has it's connector arrow in +z
;; sp3 atom has connector arrow in (dir 1 1 1)

(: atom-sp3 (-> Pict3D (-> Sym/Int Pict3D)))
(define ((atom-sp3 atom-pict) name)
  (combine
   atom-pict
   (basis (sym+ name 'root) (call-with-values v1 point-at))
   (basis (sym+ name 'f) (call-with-values v2 point-at))
   (basis (sym+ name 'l) (call-with-values v3 point-at))
   (basis (sym+ name 'r) (call-with-values v4 point-at))))
(define carbon-sp3 (atom-sp3 CARBON))

(define C-1 (carbon-sp3 'c1))
(define C-2 (carbon-sp3 'c2))
(define C-3 (carbon-sp3 'c3))
(define C-4 (carbon-sp3 'c4))
(define C-5 (carbon-sp3 'c5))
(define B-1 (bond 'c1 'c2))
(define B-2 (bond 'c2 'c3))
(define B-3 (bond 'c2 'c4))
(define B-4 (bond 'c2 'c5))

(define M-1 (join B-1 '(c1c2) C-1 '(c1root)))
(define M-2 (join M-1 '(c2c1) C-2 '(c2root)))
(define M-3 (join M-2 '(c2f) B-2 '(c2c3)))
(define M-4 (join M-3 '(c2l) B-3 '(c2c4)))
(define M-5 (join M-4 '(c2r) B-4 '(c2c5)))
(define M-6 (join M-5 '(c3c2) C-3 '(c3root)))
(define M-7 (join M-6 '(c4c2) C-4 '(c4root)))
(define METHANE (join M-7 '(c5c2) C-5 '(c5root)))
#;(explore (remove-group M-8 '(c2root c2f c2l c2r)))

(define BOND (cylinder origin (dir .125 .125 2)))
(define MINI-BOND (cylinder origin (dir .09 .09 2)))
(define TEMP-1 (combine BOND (move-x BOND 1)))
(define TEMP-2 (combine TEMP-1 (move-x BOND -1)))

(define BOND-BEND-RIGHT (bend (tessellate MINI-BOND) 90))
(define BOND-BEND-LEFT  (bend (tessellate MINI-BOND) -90))

(define PI-BOND-Y (combine (move-x BOND-BEND-RIGHT -1) (move-x BOND-BEND-LEFT 1)))
(define PI-BOND-X (rotate-z PI-BOND-Y 90))

(define DOUBLE-BOND (combine BOND PI-BOND-X))
(define TRIPLE-BOND (combine DOUBLE-BOND PI-BOND-Y))

(define ELECTRON (sphere origin .25))
(define LONE-PAIR (combine (move-x ELECTRON -.5) (move-x ELECTRON .5)))
(define SHELL (with-color (rgba "lightblue") (rotate-z (ellipsoid origin (dir 1 2 1) #:inside? #t) 90)))
(define LONE-PAIR-WITH-SHELL (combine SHELL LONE-PAIR))

(: lone-pair (-> Sym/Int Integer Pict3D))
(define (lone-pair atom num)
  (combine
   LONE-PAIR-WITH-SHELL
   (basis (sym+ atom (sym+ 'e num)) (point-at (pos 4 0 0) -x))))

(define OXYGEN (with-color (rgba "red") (sphere origin 1.25)))
(define HYDROGEN (sphere origin .5))
(define oxygen-sp3 (atom-sp3 OXYGEN))
(define hydrogen-sp3 (atom-sp3 HYDROGEN))

(define C2H4 (join (join (carbon 'c1) '(c1left) (combine (bond 'c1 'c2) PI-BOND-X) '(c1c2))
                   '(c2c1) (carbon 'c2) '(c2front)))
(define C2H2 (join (join (carbon 'c1) '(c1left) (combine (bond 'c1 'c2) PI-BOND-X PI-BOND-Y) '(c1c2))
                   '(c2c1) (carbon 'c2) '(c2front)))

;; h20

(define OH-
  (local [(define M-1 (lone-pair 'o1 1))
          (define M-2 (join M-1 '(o1e1) (oxygen-sp3 'o1) '(o1root)))
          (define M-3 (join (lone-pair 'o1 2) '(o1e2) M-2 '(o1l) ))
          (define M-4 (join M-3 '(o1r) (bond 'o1 'h1) '(o1h1)))
          (define M-5 (join M-4 '(h1o1) (hydrogen-sp3 'h1) '(h1root)))
          (define M-6 (join M-5 '(o1f) (lone-pair 'o1 3) '(o1e3)))]
    M-6))

(define H2O
  (local [(define M-1 (lone-pair 'o1 1))
          (define M-2 (join M-1 '(o1e1) (oxygen-sp3 'o1) '(o1root)))
          (define M-3 (join (lone-pair 'o1 2) '(o1e2) M-2 '(o1l) ))
          (define M-4 (join M-3 '(o1r) (bond 'o1 'h1) '(o1h1)))
          (define M-5 (join M-4 '(o1f) (bond 'o1 'h2) '(o1h2)))
          (define M-6 (join M-5 '(h1o1) (hydrogen-sp3 'h1) '(h1root)))
          (define M-7 (join M-6 '(h2o1) (hydrogen-sp3 'h2) '(h2root)))]
    M-7))

(define H3O+
  (local [(define M-1 (lone-pair 'o1 1))
          (define M-2 (join M-1 '(o1e1) (oxygen-sp3 'o1) '(o1root)))
          (define M-3 (join M-2 '(o1r) (bond 'o1 'h1) '(o1h1)))
          (define M-4 (join M-3 '(o1f) (bond 'o1 'h2) '(o1h2)))
          (define M-5 (join M-4 '(o1l) (bond 'o1 'h3) '(o1h3)))
          (define M-6 (join M-5 '(h1o1) (hydrogen-sp3 'h1) '(h1root)))
          (define M-7 (join M-6 '(h2o1) (hydrogen-sp3 'h2) '(h2root)))
          (define M-8 (join M-7 '(h3o1) (hydrogen-sp3 'h3) '(h3root)))]
    M-8))
