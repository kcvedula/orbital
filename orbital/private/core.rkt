#lang typed/racket

(provide sys-entry
         surround/tick

         send-babel
         send-babel-smiles

         set-atom
         add-bond
         compile-atoms
         compile-bonds
         compile-mol
         mol->smiles

         (struct-out atom)
         (struct-out bond)
         (struct-out mol))

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
(define sys-entry (make-parameter ""))

; utilities when using strings
(define dq "\"")
(define (surround/tick (s : String))
  (string-append "'" s "'"))

; to send the system a command, returns a pair of the standard out and standard error
(define (send-system (s : String))
  (unless (sys-entry) (error 'sys-entry "not initialized"))
  (match-define
    (list out in pid err proc)
    (process (string-append (sys-entry) " " s)))
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
(define (send-babel (s : String))
  (send-system (string-append "obabel " s)))

(define (send-babel-smiles (the-smiles : String) (the-options : String))
  (define cmd (string-append "-:" dq the-smiles dq " " the-options))
  (send-babel cmd))

(module+ test
  (send-babel-smiles "C" "-ocan"))

; data definitions and parsing to CML, as well as going to babel for smiles

; to represent an atom in a molecule. a further static check on element consulting clean-elements
; could be useful to protect from spelling errors
(struct atom [(element : Symbol)
              (mass-number : (U #f Positive-Integer))
              (chirality : (U #f 'R 'S))
              (formal-charge : (U #f Integer))]
  #:transparent)

; to represent a bond in a molecule
(struct bond [(a1 : Symbol)
              (a2 : Symbol)
              (order : Positive-Integer)
              (stereo : (U #f 'E 'Z))]
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

; <field>="<v>"
(: compile-field (-> String Any String))
(define (compile-field f v)
  (string-append f "=" "'" (format "~a" v) "'"))

(: compile-atom (-> atom Symbol String))
(define (compile-atom a id)
  (match-define (atom element mass-number chirality formal-charge) a)
  (string-append
   "<atom" " "
   (compile-field "id" id) " "
   (compile-field "elementType" element) " "
   (if mass-number (string-append (compile-field "massNumber" mass-number) " ") "")
   (if chirality (string-append (compile-field "chirality" chirality) " ") "")
   (if formal-charge (string-append (compile-field "formalCharge" formal-charge) " ") "")
   "/>"))

(module+ test
  (require typed/rackunit)
  (check-equal?
   (compile-atom
    (atom 'C 13 'R 0)
    'a1)
   (string-append
    "<atom" " "
    "id=" (surround/tick "a1") " "
    "elementType=" (surround/tick "C") " "
    "massNumber=" (surround/tick "13") " "
    "chirality=" (surround/tick "R") " "
    "formalCharge=" (surround/tick "0") " "
    "/>")))

#|
compiling the bond in a cis-alkene with two fluorine and two hydrogens,
(bond 'a1 'a2 2 'Z)
|#
(define (compile-bond b)
  (match-define (bond a1 a2 order stereo) b)
  (string-append
   "<bond" " "
   (compile-field "atomRefs2" (format "~a ~a" a1 a2)) " "
   (compile-field "order" order) " "
   (if stereo (string-append (compile-field "stereo" stereo) " ") "")
   "/>"))

(module+ test
  (check-equal?
   (compile-bond (bond 'a1 'a2 2 'Z))
   (string-append
    "<bond" " "
    "atomRefs2=" (surround/tick "a1 a2") " "
    "order=" (surround/tick "2") " "
    "stereo=" (surround/tick "Z") " "
    "/>")))

(struct mol [(atoms : (HashTable Symbol atom))
             (bonds : (Listof bond))]
  #:transparent)

(module+ test
  (define empty-mol (mol (make-immutable-hash) empty)))

(: set-atom (-> mol Symbol atom mol))
(define (set-atom m id a)
  (match-define (mol atoms bonds) m)
  (define atoms2 (hash-set atoms id a))
  (mol atoms2 bonds))

(: add-bond (-> mol bond mol))
; doesn't check if a bond between two atoms already exists
(define (add-bond m b)
  (match-define (mol atoms bonds) m)
  (mol atoms (cons b bonds)))

(module+ test
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
      m)))

(: compile-atoms (-> (HashTable Symbol atom) String))
(define (compile-atoms atoms)
  (define start "<atomArray>")
  (define end "</atomArray>")
  (define mid
    (foldl
     (λ ((id+atom : (Pair Symbol atom)) acc)
       (format "~a ~a" (compile-atom (cdr id+atom) (car id+atom)) acc))
     ""
     (hash->list atoms)))
  (string-append start " " mid " " end))

(: compile-bonds (-> (Listof bond) String))
(define (compile-bonds bonds)
  (define start "<bondArray>")
  (define end "</bondArray>")
  (define mid
    (foldl (λ (b acc) (format "~a ~a" acc (compile-bond b))) "" bonds))
  (string-append start " " mid " " end))

(: compile-mol (->* (mol) (String) String))
(define (compile-mol m [id "CH4"])
  (define compiled-atoms (compile-atoms (mol-atoms m)))
  (define compiled-bonds (compile-bonds (mol-bonds m)))
  (define start (string-append "<molecule id=" (surround/tick id) ">"))
  (define end "</molecule>")
  (string-append start " " compiled-atoms " " compiled-bonds " " end))

(: mol->smiles (-> mol String))
(define (mol->smiles m)
  (define cml (compile-mol m))

  (define cmd
    (string-append "echo " dq cml dq " | obabel -icml - -ocan"))
  (define res (send-system cmd))
  (define out (car res))
  (first (string-split out "\t")))
