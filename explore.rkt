#lang racket

(require pict3d
        pict3d/universe)

(provide explore)
(require "./gyroscope.rkt")

(define FPS 144)
(define DELTA-LOOK (degrees->radians .5))
(define DELTA-MOVE (/ 20 FPS))
(define MOVE-KEYSET (set "w" "a" "s" "d" "shift" " "))
(define LOOK-KEYSET (set "up" "down" "left" "right" "q" "e"))

(struct ws (scene pos view keyset) #:transparent)

(define (explore pict)
  (big-bang3d (init-ws pict)
            #:on-draw on-draw
            #:on-key on-key
            #:on-release on-release
            #:frame-delay (/ 1000 FPS)
            #:on-frame on-frame))

(define (init-ws pict)
  (let
      ((INIT-POS (pos 0 -10 0))
       (INIT-KEYSET (set)))
    (ws pict INIT-POS identity-linear INIT-KEYSET)))

(define (on-frame state n t)
  (match-define (ws ws.scene ws.pos ws.view ws.keyset) state)
  (match-define (linear right forward up) ws.view)
  (ws ; scene pos view keyset
   ws.scene
   (foldr
    (λ (a-dir acc) (pos+ acc a-dir DELTA-MOVE)) ; func
    ws.pos ; start
    (map (move-key->dir ws.view) (set->list (set-intersect MOVE-KEYSET ws.keyset)))); list
   (foldr look-key->view  ws.view (set->list (set-intersect LOOK-KEYSET ws.keyset)))
   ws.keyset))

(define (on-draw state n t)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (match-define (linear right forward up) ws.view)
  (combine
   ws.scene
   (sunlight forward)
   (basis 'camera (point-at ws.pos forward #:up up))
   ))

(define (on-key state n t k)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (cond 
    [(set-member? (set-union MOVE-KEYSET LOOK-KEYSET) k)
     (ws ws.scene ws.pos ws.view (set-add ws.keys k))]
    [else state]))

(define (on-release state n t k)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (cond
    [(set-member? (set-union MOVE-KEYSET LOOK-KEYSET) k)
     (ws ws.scene ws.pos ws.view (set-remove ws.keys k))]
    [else state]))


(define ((move-key->dir lt) k)
  (match-define (linear right forward up) lt)
  (match k
    ("w" forward)
    ("s" (dir-negate ((move-key->dir lt) "w")))
    ("d" right)
    ("a" (dir-negate ((move-key->dir lt) "d")))
    (" " up)
    ("shift" (dir-negate ((move-key->dir lt) " ")))
    [_ (error 'key->dir "unknown key")]))

(define (look-key->view k lt)
  (match k
    ("e" (roll-right DELTA-LOOK lt))
    ("right" (look-right DELTA-LOOK lt))
    ("up" (look-up DELTA-LOOK lt))
    ("q" (roll-right (- DELTA-LOOK) lt))
    ("left" (look-right (- DELTA-LOOK) lt))
    ("down" (look-up (- DELTA-LOOK) lt)) 
    (_ (error `look-key->view "bad look key"))))

(current-pict3d-fov 60)

