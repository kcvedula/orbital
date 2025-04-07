#lang racket

(provide
 explore-smiles
 smiles->pict3d)
         
(require "core.rkt")
(require "periodic-table.rkt")
(require "explore.rkt")
(require pict3d)
(require net/http-client)
(require net/url)
#|
obabel -:"CCO" -osdf --gen3d

 OpenBabel04062517513D

  9  8  0  0  0  0  0  0  0  0999 V2000
    1.0762   -0.0258   -0.0703 C   0  0  0  0  0  0  0  0  0  0  0  0
    2.5905   -0.0186   -0.0697 C   0  0  0  0  0  0  0  0  0  0  0  0
    3.0722    1.0423    0.7418 O   0  0  0  0  0  0  0  0  0  0  0  0
    0.6914   -0.8450   -0.6840 H   0  0  0  0  0  0  0  0  0  0  0  0
    0.6842    0.9198   -0.4589 H   0  0  0  0  0  0  0  0  0  0  0  0
    0.6907   -0.1358    0.9487 H   0  0  0  0  0  0  0  0  0  0  0  0
    2.9819    0.1090   -1.0837 H   0  0  0  0  0  0  0  0  0  0  0  0
    2.9775   -0.9589    0.3335 H   0  0  0  0  0  0  0  0  0  0  0  0
    2.7326    1.8740    0.3695 H   0  0  0  0  0  0  0  0  0  0  0  0
  1  2  1  0  0  0  0
  1  4  1  0  0  0  0
  1  5  1  0  0  0  0
  1  6  1  0  0  0  0
  2  3  1  0  0  0  0
  2  7  1  0  0  0  0
  2  8  1  0  0  0  0
  3  9  1  0  0  0  0
M  END
|#
#|
How to work with a sdf string
2. split by "\n"
3. throw away the first two lines
4. the first two numbers of the now first line contain #atoms #bonds
5. make two lists with the subsequenent #atoms atoms and #bonds bonds
|#

(define (sdf->lines/header x)
  (let* ((x x)
         (x (string-split x "\n"))
         (x (drop x 2)))
         x))

(define (parse-header h)
  (map string->number (take (string-split h) 2)))

(define (lines/header->raw-atoms&bonds x)
  (define amounts (parse-header (car x)))
  (define atoms (take (cdr x) (first amounts)))
  (define bonds (take (drop (cdr x) (first amounts)) (second amounts)))
  (cons atoms bonds))

; structures to represent atoms and bonds for rendering
(struct atom3d (id element x y z) #:transparent)

(struct bond3d (a1 a2 order) #:transparent)

(define (clean-raw-atom a id)
  (define important (take (string-split a) 4))
  (atom3d id
          (string->symbol (fourth important))
          (string->number (first important))
          (string->number (second important))
          (string->number (third important))))

(define (clean-raw-bond b)
  (define important (take (string-split b) 3))
  (bond3d (string->number (first important))
          (string->number (second important))
          (string->number (third important))))

(define (clean-raw-atoms atoms [id 1])
  (if (empty? atoms)
      '()
      (cons (clean-raw-atom (car atoms) id)
            (clean-raw-atoms (cdr atoms) (add1 id)))))

(define (clean-raw-atoms&bonds r)
  (define r-atoms (car r))
  (define r-bonds (cdr r))
  (define c-atoms (clean-raw-atoms r-atoms))
  (define c-bonds (map clean-raw-bond r-bonds))
  (cons c-atoms c-bonds))

(define (raw-sdf->clean-atoms&bonds x0)
  (define x1 (sdf->lines/header x0))
  (define x2 (lines/header->raw-atoms&bonds x1))
  (clean-raw-atoms&bonds x2))

;(raw-sdf->clean-atoms&bonds ex-data)

; rendering

(define (element/symbol s)
  (define res
    (filter
     (λ (e) (equal? (element-symbol e) s))
     clean-elements))
  (if (= (length res) 1) (car res) (error 'element/symbol "no match")))



(define ATOMIC-RADIUS-SCALE (make-parameter .0025))

(define (atom3d->pict3d a)
  (match-define (atom3d id elem-sym x y z) a)
  (define elem (element/symbol elem-sym))
  (define rad (* (ATOMIC-RADIUS-SCALE)
                 (element-atomic-radius elem)))
  (combine 
   (with-color
      (rgba (if (element-cpk-color elem)
                (element-cpk-color elem)
                (rgba "black")))
    (sphere (pos x y z) rad))
   (basis id (move (dir x y z)))))

(define (atoms3d->pict3d as)
  (apply combine (map atom3d->pict3d as)))

(define BOND-RADIUS (make-parameter .08))

; draws a cylinder between two points with radius r
(define (bond-pict p1 p2 [order 1] [rad (BOND-RADIUS)])
  (define l/2 (/ (pos-dist p1 p2) 2))
  (define cyl (move-z (cylinder origin (dir rad rad l/2)) l/2))
  (define d (* rad 1.5)) ; bigger than radius
  (transform
   (cond [(equal? order 1) cyl]
        [(equal? order 2) (combine (move-x cyl d)
                                   (move-x cyl (- d)))]
        [(equal? order 3) (combine (move-x cyl d)
                                   cyl
                                   (move-x cyl (- d)))]
        [else (error "higher order bonds not supported: " order)])
   (point-at p1 p2)))
   

(define (add-bond p b)
  (match-define (bond3d a1 a2 order) b) ; TODO handle order correctly
  (define p1 (affine-origin (find-group-transform p (list a1))))
  (define p2 (affine-origin (find-group-transform p (list a2))))
  (combine p (bond-pict p1 p2 order)))

(define (add-bonds p bs)
  (foldl (λ (b acc) (add-bond acc b)) p bs))

(define (atoms&bonds->pict3d x)
  (define atoms (car x))
  (define bonds (cdr x))
  (define p (atoms3d->pict3d atoms))
  (add-bonds p bonds))

(define (smiles->sdf s)
  (car (send-babel-smiles
        s
        "-osdf --gen3d --fastest")))

(define (smiles->pict3d s)
  (define raw (smiles->sdf s))
  (define clean (raw-sdf->clean-atoms&bonds raw))
  (freeze (atoms&bonds->pict3d clean)))

(define (mol->pict3d m)
  (smiles->pict3d (mol->smiles m)))

(define (explore-smiles s)
  (explore (smiles->pict3d s)))

(define (explore-pid pid)
  (explore-smiles (pid->smiles pid)))

(define (pid->smiles pid)
  (define PT-URL
    (string->url
     (string-append
     "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/" (number->string pid)  "/property/CanonicalSMILES/TXT")))

  (define conn-to-pubchem (http-conn-open (url-host PT-URL) #:ssl? #t))

  (match-define-values (a b in) (http-conn-sendrecv! conn-to-pubchem  (url->string PT-URL))) ; input is a gzip

  (define res (port->string in))
  (http-conn-close! conn-to-pubchem)
  (string-trim res))

