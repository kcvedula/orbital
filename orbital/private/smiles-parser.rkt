#lang racket

(require parser-tools/lex
         parser-tools/yacc)

;; Tokens with values
(define-tokens value-tokens (NUMBER DIGIT))

;; Tokens without values
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

;; Lexer
(define smiles-lexer
  (lexer-src-pos
   [(eof) (token-EOF)]
   [#\[ (token-LBRACKET)]
   [#\] (token-RBRACKET)]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\. (token-DOT)]
   [#\* (token-STAR)]
   [#\+ (token-PLUS)]
   [#\- (token-MINUS)]
   [#\: (token-COLON)]
   [#\% (token-PERCENT)]
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
   [(repetition 1 +inf.0 numeric) (token-NUMBER (string->number lexeme))]
   [numeric (token-DIGIT (string->number lexeme))]))

;; Parser
(define smiles-parser
  (parser
   (src-pos)
   (start smiles)
   (end EOF)
   (tokens value-tokens tokens)
   (error (lambda (tok? name val start end)
            (error "Parse error on token" tok? name val start end)))

   (grammar
    [smiles ((chain) $1)]

    ;; Chains and branching
    [chain ((branched-atom) (list $1))
           ((chain branched-atom) (append $1 (list $2)))
           ((chain bond branched-atom) (append $1 (list (list 'bond $2) $3)))
           ((chain dot branched-atom) (append $1 (list (list 'bond '\.) $3)))]

    [branched-atom ((atom branches) `(branched ,$1 ,$2))
                   ((atom) `(branched ,$1 '()))]

    [branches ((branch branches) (cons $1 $2))
              ((branch) (list $1))]

    [branch ((LPAREN chain RPAREN) `(branch ,$2))]

    ;; Atoms
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

    ;; Bracket atom
    [bracket-atom ((LBRACKET isotope? symbol chiral? hcount? charge? class? RBRACKET)
                   `(bracket ,@(filter values (list $2 $3 $4 $5 $6 $7))))]

    [isotope? ((NUMBER) `(isotope ,$1)) (() #f)]
    [symbol ((aliphatic-organic) `(symbol ,$1))
            ((aromatic-organic)  `(symbol ,$1))
            ((STAR) '(symbol *))]

    [chiral? ((AT) '(chiral @))
             ((AT_AT) '(chiral @@))
             (() #f)]

    [hcount? ((H_COUNT NUMBER) `(hcount ,$2)) 
             ((H_COUNT) '(hcount 1)) (() #f)]

    [charge? ((PLUS DIGIT) `(charge ,$2))
             ((PLUS) '(charge 1))
             ((MINUS DIGIT) `(charge ,(- $2)))
             ((MINUS) '(charge -1))
             (() #f)]

    [class? ((COLON NUMBER) `(class ,$2)) (() #f)]

    [bond ((BOND_SINGLE) '-) ((BOND_DOUBLE) '=) ((BOND_TRIPLE) '\#)
          ((BOND_QUADRUPLE) '$) ((BOND_AROMATIC) ':)
          ((BOND_UP) '/) ((BOND_DOWN) '\\)]

    [dot ((DOT) '\.)])))

;; Entry point
(define (parse-smiles str)
  (define in (open-input-string str))
  (define (next-token) (smiles-lexer in))
  (smiles-parser next-token))

(module+ main
  (for ([s '("C" "CC" "C(C)C" "[13CH3]" "[O-]" "C=C" "F/C=C/F")])
    (printf "~a\n=>\n" s)
    (pretty-print (parse-smiles s))
    (newline)))
