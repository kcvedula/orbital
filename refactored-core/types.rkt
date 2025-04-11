#lang racket

(provide (all-defined-out))

(require (only-in racket/draw color%)
         pict/convert)

; types for periodic table
(struct/contract an-element-symbol ((v symbol?)) #:transparent)

(struct/contract element
  [(atomic-number (between/c 1 118)) 
   (symbol an-element-symbol?) 
   (name symbol?) 
   (atomic-mass number?) 
   (cpk-color (or/c (is-a?/c color%) #f)) 
   (electron-configuration list?)
   (electronegativity (or/c number? #f))
   (atomic-radius (or/c number? #f))
   (ionization-energy (or/c number? #f))
   (electron-affinity (or/c number? #f)) 
   (oxidation-states (listof number?)) 
   (standard-state symbol?)
   (melting-point (or/c number? #f))
   (boiling-point (or/c number? #f))
   (density (or/c number? #f)) 
   (group-block string?) 
   (year-discovered (or/c number? #f))] 
  #:transparent)

; types for molecules
(struct/contract
 atom
 ((element an-element-symbol?)
  (mass-number (or/c #f positive-integer?))
  (chirality (or/c #f 'R 'S))
  (formal-charge (or/c #f integer?)))
 #:transparent)

(struct/contract
 bond
 ((id1 positive-integer?)
  (id2 positive-integer?)
  (order (or/c 1 2 3))
  (stereo (or/c #f 'E 'Z)))
 #:transparent)

(struct/contract
 mol
 ((atoms (hash/c positive-integer? atom?))
  (bonds (listof bond?)))
 #:transparent)

(struct/contract
 template
 mol
 ((next-id positive-integer?)
  (bonding-atoms-ids (listof positive-integer?)))
 #:transparent)

(struct/contract
  substituent
  template
  ()
  #:transparent)

(struct/contract
  info-substituent-addition
  ((substituent substituent?)
   (bond bond?)
   (num-times (between/c 1 8)))
  #:transparent)


; types for babel

(struct/contract smiles ((v string?)) #:transparent)

(struct/contract cml ((v string?)) #:transparent)

(struct/contract sdf ((v string?)) #:transparent)

(struct/contract png ((v bytes?)) #:transparent)

; types for pubchem

(struct/contract cid ((v positive-integer?)) #:transparent)

(struct/contract conformer ((v string?)) #:transparent)


; types for 3d generation

(struct/contract
 atom3d
 ((id positive-integer?)
  (element (and/c natural? (between/c 1 118)))
  (x real?)
  (y real?)
  (z real?))
 #:transparent)

(struct/contract
 bond3d
 ((a1 positive-integer?)
  (a2 positive-integer?)
  (order (or/c 1 2 3)))
 #:transparent)

(struct/contract
 mol3d
 ((atoms3d (listof atom3d?))
  (bonds3d (listof bond3d?)))
 #:transparent)

; types for networking

(struct https-get-resp (status headers raw) #:transparent)
