#lang racket

(provide (all-defined-out))

#|
TODO: SYNTAX LAYER FOR MOLECULES

Summary of core library:
We represent molecules broadly as an undirected weighted graph
with [atom?] vertices and [bond?] edges.
We compile to CML, an XML like markup language to hold chemical information.
From there, through obabel, we can convert to any of their suite to facilitate rendering
and get other insights.

The first portion of the core is an abstraction over system commands.
The second portion of the core is defining our molecules, atoms, and bonds and compiling them to CML.
|#

; entry to whatever system you want to use, here incase someone needs wsl or some special shell
(define sys-entry (make-parameter #f))
(sys-entry "")

; utilities when using strings
(define sp " ")
(define dq "\"")
(define (surround/tick s)
  (string-append "'" s "'"))

; to send the system a command, returns a pair of the standard out and standard error
(define (send-system s)
  (unless (sys-entry) (error 'sys-entry "not initialized"))
  (match-define
     (list out in pid err proc)
    (process (string-append (sys-entry) sp s)))
  (proc 'wait)
  (define res
    (list
     (string-trim (port->string out))
     (string-trim (port->string err))))
  ; cleanup
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  res)

; to send babel a command
(define (send-babel s)
  (send-system (string-append "obabel " s)))

(define (send-babel-smiles the-smiles the-options)
  (define cmd (string-append "-:" dq the-smiles dq sp the-options))
  (send-babel cmd))


; data definitions and parsing to CML, as well as going to babel for smiles

; to represent an atom in a molecule. a further static check on element consulting clean-elements
; could be useful to protect from spelling errors
(struct/contract
 atom
 ((element symbol?)
  (mass-number (or/c #f positive-integer?))
  (chirality (or/c #f 'R 'S))
  (formal-charge (or/c #f integer?)))
 #:transparent)

; to represent a bond in a molecule
(struct/contract
 bond
 ((a1 symbol?)
  (a2 symbol?)
  (order positive-integer?)
  (stereo (or/c #f 'E 'Z)))
 #:transparent)

#|
Comping an atom? into cml for an atom:
A complex example, compiling the carbon13 in

    H
    |
    C-13
  / | \
 F  I Cl

put H in the back, I -> Cl -> F is clockwise, so this is an R configuration
(atom 'C 13 'R 0)
=>
<atom
id="a1"
elementType="C"
massNumber="13"
chirality="R"
formalCharge="0"
/>
|#

; (-> string? any? string?)
; <field>="<v>"
(define (compile-field f v)
  (string-append f "=" "'" (format "~a" v) "'"))



(define (compile-atom a id)
  (match-define (atom element mass-number chirality formal-charge) a)
  (string-append
   "<atom" sp
   (compile-field "id" id) sp
   (compile-field "elementType" element) sp
   (if mass-number (string-append (compile-field "massNumber" mass-number) sp) "")
   (if chirality (string-append (compile-field "chirality" chirality) sp) "")
   (if formal-charge (string-append (compile-field "formalCharge" formal-charge) sp) "")
   "/>"))

(module+ test
  (require rackunit)
  (check-equal?
   (compile-atom
            (atom 'C 13 'R 0)
            'a1)
   (string-append
    "<atom" sp
    "id=" (surround/tick "a1") sp
    "elementType=" (surround/tick "C") sp
    "massNumber=" (surround/tick "13") sp
    "chirality=" (surround/tick "R") sp
    "formalCharge=" (surround/tick "0") sp
    "/>")))

#|
compiling the bond in a cis-alkene with two fluorine and two hydrogens,
(bond 'a1 'a2 2 'Z)
|#
(define (compile-bond b)
  (match-define (bond a1 a2 order stereo) b)
  (string-append
   "<bond" sp
   (compile-field "atomRefs2" (format "~a ~a" a1 a2)) sp
   (compile-field "order" order) sp 
   (if stereo (string-append (compile-field "stereo" stereo) sp) "")
   "/>"))

(module+ test
  (check-equal?
   (compile-bond (bond 'a1 'a2 2 'Z))
                 (string-append
                  "<bond" sp
                  "atomRefs2=" (surround/tick "a1 a2") sp
                  "order=" (surround/tick "2") sp
                  "stereo=" (surround/tick "Z") sp
                  "/>")))

(struct/contract
 mol
 ((atoms (hash/c symbol? atom?))
  (bonds (listof bond?)))
 #:transparent)

(define empty-mol (mol (make-immutable-hash) empty))

(define (set-atom m id a)
  (match-define (mol atoms bonds) m)
  (define atoms2 (hash-set atoms id a))
  (mol atoms2 bonds))

; doesn't check if a bond between two atoms already exists
(define (add-bond m b)
  (match-define (mol atoms bonds) m)
  (mol atoms (cons b bonds)))

; CH4 example
(define H (atom 'H #f #f #f))
(define C (atom 'C #f #f #f))

(define CH4
  (let* ((m empty-mol)
         (m (set-atom m 'a1 C))
         (m (set-atom m 'a2 H))
         (m (set-atom m 'a3 H))
         (m (set-atom m 'a4 H))
         (m (set-atom m 'a5 H))
         (m (add-bond m (bond 'a1 'a2 1 #f)))
         (m (add-bond m (bond 'a1 'a3 1 #f)))
         (m (add-bond m (bond 'a1 'a4 1 #f)))
         (m (add-bond m (bond 'a1 'a5 1 #f))))
  m))

(define (compile-atoms atoms)
  (define start "<atomArray>")
  (define end "</atomArray>")
  (define mid
    (foldl
     (λ (id+atom acc)
       (format "~a ~a" (compile-atom (cdr id+atom) (car id+atom)) acc))
     ""
     (hash->list atoms)))
  (string-append start sp mid sp end))

(define (compile-bonds bonds)
  (define start "<bondArray>")
  (define end "</bondArray>")
  (define mid
    (foldl (λ (b acc) (format "~a ~a" acc (compile-bond b))) "" bonds))
  (string-append start sp mid sp end))


(define (compile-mol m [id "CH4"])
  (define compiled-atoms (compile-atoms (mol-atoms m)))
  (define compiled-bonds (compile-bonds (mol-bonds m)))
  (define start (string-append "<molecule id=" (surround/tick id) ">"))
  (define end "</molecule>")
  (string-append start sp compiled-atoms sp compiled-bonds sp end))



(define (mol->smiles m)
  (define cml (compile-mol m))
 
  (define cmd
    (string-append "wsl echo " dq cml dq " | obabel -icml - -ocan"))
  (define res (send-system cmd))
  (display res)
  (define out (car res))
  (first (string-split out "\t")))

