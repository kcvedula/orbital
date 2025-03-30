#lang racket

(require "explore.rkt")
(require "functional-groups.rkt")
(require "orbital1.rkt")
(require pict3d)


#; (explore (sp3 OXYGEN #:d1 lone-pair #:d2 lone-pair #:d3 -H #:R -H))
(explore (aryl #:d1 -Iodine #:d2 -Cl #:d3 -Br #:d4 (carbon-chain 8) #:R -H))

