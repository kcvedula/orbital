#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

;; <smiles>            ::= <chain>
;; <chain>             ::= <bond> <branched-atom>
;;                      |  <chain> <ring-bond>
;;                      |  <chain> <bond> <ring-bond>
;;                      |  <branched-atom>
;;                      |  <chain> <branched-atom>
;;                      |  <chain> <bond> <branched-atom>
;;                      |  <chain> '.' <branched-atom>
;; <ring-bond>         ::= <digit>
;;                      |  '%' <number>
;; <branched-atom>     ::= <atom> <branches>
;;                      |  <atom>
;; <branches>          ::= <branch>
;;                      |  <branch> <branches>
;; <branch>            ::= '(' <chain> ')'
;; <atom>              ::= <bracket-atom>
;;                      |  <aliphatic-organic>
;;                      |  <aromatic-organic>
;;                      |  '*'
;; <aliphatic-organic> ::= 'B' | 'C' | 'N' | 'O' | 'S' | 'P' | 'F' | 'Cl' | 'Br' | 'I'
;; <aromatic-organic>  ::= 'b' | 'c' | 'n' | 'o' | 's' | 'p' | 'se' | 'as'
;; <bracket-atom>      ::= '[' <isotope>? <symbol> <chiral>? <hcount>? <charge>? <class>? ']'
;; <isotope>           ::= <number>
;;                      |  <digit>
;; <symbol>            ::= <aliphatic-organic>
;;                      |  <aromatic-organic>
;;                      |  '*'
;; <chiral>            ::= '@'
;;                      |  '@@'
;; <hcount>            ::= 'H' <number>
;;                      |  'H' <digit>
;;                      |  'H'
;; <charge>            ::= '+' <digit>
;;                      |  '+'
;;                      |  '-' <digit>
;;                      |  '-'
;; <class>             ::= ':' <number>
;;                      |  ':' <digit>
;; <bond>              ::= '-'   ; single bond
;;                      |  '='   ; double bond
;;                      |  '#'   ; triple bond
;;                      |  '$'   ; quadruple bond
;;                      |  ':'   ; aromatic bond
;;                      |  '/'   ; up bond
;;                      |  '\\'  ; down bond
;; <digit>             ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
;; <number>            ::= <digit> <digit>*

(define-tokens value-tokens (NUMBER DIGIT SYMBOL))

(define inside-brackets? (make-parameter #f))


(define elements
  '(H He Li Be B C N O F Ne Na Mg Al Si P S Cl Ar K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr
      Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm
      Yb Lu Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn Fr Ra Ac Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No
      Lr Rf Db Sg Bh Hs Mt Ds Rg Cn Nh Fl Mc Lv Ts Og))

(define-empty-tokens tokens
  (LBRACKET RBRACKET LPAREN RPAREN
            DOT STAR PLUS MINUS COLON PERCENT
            AT AT_AT H_COUNT
            ALIPHATIC_B ALIPHATIC_C ALIPHATIC_N ALIPHATIC_O ALIPHATIC_S ALIPHATIC_P
            ALIPHATIC_F ALIPHATIC_CL ALIPHATIC_BR ALIPHATIC_I
            AROMATIC_B AROMATIC_C AROMATIC_N AROMATIC_O AROMATIC_S AROMATIC_P
            AROMATIC_SE AROMATIC_AS
            BOND_SINGLE BOND_DOUBLE BOND_TRIPLE BOND_QUADRUPLE BOND_AROMATIC BOND_UP BOND_DOWN
            EOF))

(define smiles-lexer
  (lexer
   [(eof) (token-EOF)]
   [#\[ (begin (inside-brackets? #t) (token-LBRACKET))]
   [#\] (begin (inside-brackets? #f) (token-RBRACKET))]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\. (token-DOT)]
   [#\* (token-STAR)]
   [#\+ (token-PLUS)]
   [#\- (token-MINUS)]
   [#\: (token-COLON)]
   ["@@" (token-AT_AT)]
   [#\@ (token-AT)]
   [#\H (token-H_COUNT)]
   ["Cl" (token-ALIPHATIC_CL)]
   ["Br" (token-ALIPHATIC_BR)]
   ["se" (token-AROMATIC_SE)]
   ["as" (token-AROMATIC_AS)]
   ["b" (token-AROMATIC_B)]
   ["c" (token-AROMATIC_C)]
   ["n" (token-AROMATIC_N)]
   ["o" (token-AROMATIC_O)]
   ["s" (token-AROMATIC_S)]
   ["p" (token-AROMATIC_P)]
   ["B" (token-ALIPHATIC_B)] ["C" (token-ALIPHATIC_C)] ["N" (token-ALIPHATIC_N)]
   ["O" (token-ALIPHATIC_O)] ["S" (token-ALIPHATIC_S)] ["P" (token-ALIPHATIC_P)]
   ["F" (token-ALIPHATIC_F)] ["I" (token-ALIPHATIC_I)]
   ["=" (token-BOND_DOUBLE)]
   ["#" (token-BOND_TRIPLE)]
   ["$" (token-BOND_QUADRUPLE)]
   ["/" (token-BOND_UP)]
   ["\\" (token-BOND_DOWN)]
   ["-" (token-BOND_SINGLE)]
   [(:: upper-case (:? lower-case))
    (if (member (string->symbol lexeme) elements)
        (token-SYMBOL lexeme)
        (begin
          (printf "Skipping invalid element: ~a\n" lexeme)
          (smiles-lexer input-port)))]
   
   ;; Multi-digit numbers in brackets only (e.g., 13, 18, 100)
   [(repetition 2 +inf.0 numeric)
    (if (inside-brackets?)
        (token-NUMBER (string->number lexeme))
        (let* ([first (substring lexeme 0 1)]
               [rest  (substring lexeme 1)]
               [rest+port (open-input-string (string-append rest (port->string input-port)))])
          (set! input-port rest+port)
          (token-DIGIT (string->number first))))]

   ;; Single-digit numbers always valid
   [numeric (token-DIGIT (string->number lexeme))]

   ;; Percent-prefixed ring numbers: always NUMBER after %
   [(:seq "%" (:* numeric))
    (let* ([digits (substring lexeme 1)]
           [num (string->number digits)])
      (values
       (token-PERCENT)
       (token-NUMBER num)))]))

(define smiles-parser
  (parser
   (start smiles)
   (end EOF)
   (tokens value-tokens tokens)
   (error (lambda (tok? name val)
            (error "Parse error on token" tok? name val)))

   (grammar
    [smiles ((chain) $1)]

    [chain ((bond branched-atom) (list (list 'bond $1) $2)) 
           ((branched-atom) (list $1))
           ((chain ring-bond) (append $1 (list $2)))
           ((chain bond ring-bond) (append $1 (list (list 'bond $2) $3))) 
           ((chain branched-atom) (append $1 (list $2)))
           ((chain bond branched-atom) (append $1 (list (list 'bond $2) $3)))
           ((chain dot branched-atom) (append $1 (list (list 'bond '\.) $3)))]

    [ring-bond ((DIGIT) `(ring ,$1))
               ((PERCENT NUMBER) `(ring ,$2))]

    [branched-atom ((atom branches) `(branched ,$1 ,$2))
                   ((atom) `(branched ,$1 '()))]

    [branches ((branch branches) (cons $1 $2))
              ((branch) (list $1))]

    [branch ((LPAREN chain RPAREN) `(branch ,$2))]

    [atom ((bracket-atom) $1)
          ((aliphatic-organic) (list 'aliphatic $1))
          ((aromatic-organic)  (list 'aromatic $1))
          ((STAR) '*)]

    [aliphatic-organic ((ALIPHATIC_B) 'B) ((ALIPHATIC_C) 'C) ((ALIPHATIC_N) 'N)
                       ((ALIPHATIC_O) 'O) ((ALIPHATIC_S) 'S) ((ALIPHATIC_P) 'P)
                       ((ALIPHATIC_F) 'F) ((ALIPHATIC_CL) 'Cl) ((ALIPHATIC_BR) 'Br)
                       ((ALIPHATIC_I) 'I)]

    [aromatic-organic ((AROMATIC_B) 'b) ((AROMATIC_C) 'c) ((AROMATIC_N) 'n)
                      ((AROMATIC_O) 'o) ((AROMATIC_S) 's) ((AROMATIC_P) 'p)
                      ((AROMATIC_SE) 'se) ((AROMATIC_AS) 'as)]

    [bracket-atom ((LBRACKET isotope? symbol chiral? hcount? charge? class? RBRACKET)
                   `(bracket ,@(filter values (list $2 $3 $4 $5 $6 $7))))]


    [isotope? ((NUMBER) `(isotope ,$1))
              ((DIGIT) `(isotope ,$1))
              (() #f)]
    
    [symbol ((SYMBOL) `(symbol ,$1))
            ((aliphatic-organic) `(symbol ,$1))
            ((aromatic-organic)  `(symbol ,$1))
            ((STAR) '(symbol *))]

    [chiral? ((AT) '(chiral @))
             ((AT_AT) '(chiral @@))
             (() #f)]

    [hcount? ((H_COUNT NUMBER) `(hcount ,$2))
             ((H_COUNT DIGIT) `(hcount ,$2))
             ((H_COUNT) '(hcount 1))
             (() #f)]

    [charge? ((PLUS DIGIT) `(charge ,$2))
             ((PLUS) '(charge 1))
             ((MINUS DIGIT) `(charge ,(- $2)))
             ((MINUS) '(charge -1))
             (() #f)]

    [class? ((COLON NUMBER) `(class ,$2))
            ((COLON DIGIT) `(class ,$2))
            (() #f)]

    [bond ((BOND_SINGLE) '-) ((BOND_DOUBLE) '=) ((BOND_TRIPLE) '\#)
          ((BOND_QUADRUPLE) '$) ((BOND_AROMATIC) ':)
          ((BOND_UP) '/) ((BOND_DOWN) '\\)]

    [dot ((DOT) '\.)])))

(define (parse-smiles str)
  (define in (open-input-string str))
  (define (next-token) (smiles-lexer in))
  (smiles-parser next-token))

(module+ test
  (for ([s '("C" "CC" "C(C)C" "[13CH3]" "[O-]" "C=C" "F/C=C/F" 
                 "C1CCC1" "CC(=O)CC" "CCCCCCCC" "CCCC1CC1"
                 "COc(c1)cccc1C#N"
                 "N1CCN(CC1)C(C(F)=C2)=CC(=C2C4=O)N(C3CC3)C=C4C(=O)O"
                 
                 "N#N" ; Dinitrogen
                 "CN=C=O" ; Methyl isocyanate
                 "[Cu+2].[O-]S(=O)(=O)[O-]" ; Copper(II) sulfate
                 "O=Cc1ccc(O)c(OC)c1" ; Vanillin
                 "COc1cc(C=O)ccc1O"
                 "CC(=O)NCCC1=CNc2c1cc(OC)cc2" ; Melatonin
                 "CC(=O)NCCc1c[nH]c2ccc(OC)cc12"
                 "CCc(c1)ccc2[n+]1ccc3c2[nH]c4c3cccc4" ; Flavopereirin 
                 "CCc1c[n+]2ccc3c4ccccc4[nH]c3c2cc1"
                 "CN1CCC[C@H]1c2cccnc2" ; Nicotine
                 "CCC[C@@H](O)CC\\C=C\\C=C\\C#CC#C\\C=C\\CO" ; Oenanthotoxin 
                 "CCC[C@@H](O)CC/C=C/C=C/C#CC#C/C=C/CO"
                 "CC1=C(C(=O)C[C@@H]1OC(=O)[C@@H]2[C@H](C2(C)C)/C=C(\\C)/C(=O)OC)C/C=C\\C=C" ; Pyrethrin II
                 "O1C=C[C@H]([C@H]1O2)c3c2cc(OC)c4c3OC(=O)C5=C4CCC(=O)5" ; Aflatoxin B_1


                 )])
    (printf "~a\n=>\n" s)
    (pretty-print (parse-smiles s))
    (newline)))
