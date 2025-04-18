#lang racket

(provide
 (contract-out
  (smiles->cid (-> smiles? cid?))
  (cid->smiles (-> cid? smiles?))
  (cid->mol3d (-> cid? mol3d?))))

(require "types.rkt"
         "https-get.rkt"
         json)

(define (smiles->cid s)
  (match-define (https-get-resp _ _ raw)
    (https-get
     (string-append
      "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/smiles/"
      (smiles-v s)
      "/cids/TXT")
     port->string))
  ((compose cid string->number string-trim) raw))

(define (cid->smiles c)
  (match-define (https-get-resp _ _ raw)
    (https-get
     (string-append
      "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/"
      (number->string (cid-v c))
      "/property/CanonicalSMILES/TXT")
     port->string))
  ((compose smiles string-trim) raw))

(define (cid->raw-j3d c)
  (match-define (https-get-resp _ _ raw)
    (https-get
     (string-append
      "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/"
      (number->string (cid-v c)) "/record/JSON?record_type=3d")
     read-json))
  raw)

(define x1 (cid->raw-j3d (cid 1)))

; dive into the relevant dictionary
(define (dive-j3d j3d)
  (first (hash-ref j3d 'PC_Compounds)))

(define x2 (dive-j3d x1))

; assumes dive-j3d
(define (j3d->bonds3d j3d)
  (define bonds3djson
    (hash-ref j3d 'bonds))
  (map bond3d
       (hash-ref bonds3djson 'aid1)
       (hash-ref bonds3djson 'aid2)
       (hash-ref bonds3djson 'order)))

; assumes dive-j3d
(define (j3d->atoms3d j3d)
  (define atoms3djson
    (hash-ref j3d 'atoms))
  (define coords3djson
    (first (hash-ref (first (hash-ref j3d 'coords)) 'conformers)))
  (define xs (hash-ref coords3djson 'x))
  (define ys (hash-ref coords3djson 'y))
  (define zs (hash-ref coords3djson 'z))
  (map atom3d
       (hash-ref atoms3djson 'aid)
       (hash-ref atoms3djson 'element)
       xs
       ys
       zs))

(define (cid->mol3d c)
  (define j3d (dive-j3d (cid->raw-j3d c)))
  (mol3d (j3d->atoms3d j3d) (j3d->bonds3d j3d)))
