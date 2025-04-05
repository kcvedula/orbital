#lang typed/racket

(require pict3d
         pict3d/universe
         "gyroscope.rkt")

(provide explore)

(define FPS 144)
(define DELTA-LOOK (degrees->radians .5))
(define DELTA-MOVE (/ 20 FPS))
(define MOVE-KEYSET (set "w" "a" "s" "d" "shift" " "))
(define LOOK-KEYSET (set "up" "down" "left" "right" "q" "e"))

(struct ws [(scene : Pict3D)
            (pos : Pos)
            (view : Linear)
            (keyset : (Setof String))]
  #:transparent)
(define-type WS ws)

(: explore (-> Pict3D WS))
;; Launches a 3D big bang instance
(define (explore pict)
  (big-bang3d (init-ws pict)
              #:on-draw on-draw
              #:on-key on-key
              #:on-release on-release
              #:frame-delay (/ 1000 FPS)
              #:on-frame on-frame
              #:name "Orbital"))

(: init-ws (-> Pict3D WS))
(define (init-ws pict)
  (let ((INIT-POS (pos 0 -10 0))
        (INIT-KEYSET : (Setof String) (set)))
    (ws pict INIT-POS identity-linear INIT-KEYSET)))

(: on-frame (-> WS Natural Flonum WS))
(define (on-frame state n t)
  (match-define (ws ws.scene ws.pos ws.view ws.keyset) state)
  (match-define (linear right forward up) ws.view)
  (define new-pos : Pos
    (for/fold ([acc ws.pos])
              ([key (in-set (set-intersect MOVE-KEYSET ws.keyset))])
      (pos+ acc (move-key->dir ws.view key) DELTA-MOVE)))
  (define new-view : Linear
    (for/fold ([view ws.view]) ([key (in-set (set-intersect LOOK-KEYSET ws.keyset))])
      (look-key->view key view)))
  (ws ws.scene new-pos new-view ws.keyset))

(: on-draw (-> WS Natural Flonum Pict3D))
(define (on-draw state n t)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (match-define (linear right forward up) ws.view)
  (combine
   ws.scene
   (sunlight forward)
   (basis 'camera (point-at ws.pos forward #:up up))))

(: on-key (-> WS Natural Flonum String WS))
(define (on-key state n t k)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (cond
    [(set-member? (set-union MOVE-KEYSET LOOK-KEYSET) k)
     (ws ws.scene ws.pos ws.view (set-add ws.keys k))]
    [else state]))

(: on-release (-> WS Natural Flonum String WS))
(define (on-release state n t k)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (cond
    [(set-member? (set-union MOVE-KEYSET LOOK-KEYSET) k)
     (ws ws.scene ws.pos ws.view (set-remove ws.keys k))]
    [else state]))

(: move-key->dir (-> Linear String Dir))
(define (move-key->dir lt k)
  (match-define (linear right forward up) lt)
  (match k
    ("w"     forward)
    ("s"     (dir-negate (move-key->dir lt "w")))
    ("d"     right)
    ("a"     (dir-negate (move-key->dir lt "d")))
    (" "     up)
    ("shift" (dir-negate (move-key->dir lt " ")))
    [_       (error 'key->dir "unknown key")]))

(: look-key->view (-> String Linear Linear))
(define (look-key->view k lt)
  (match k
    ("e"     (roll-right DELTA-LOOK lt))
    ("right" (look-right DELTA-LOOK lt))
    ("up"    (look-up DELTA-LOOK lt))
    ("q"     (roll-right (- DELTA-LOOK) lt))
    ("left"  (look-right (- DELTA-LOOK) lt))
    ("down"  (look-up (- DELTA-LOOK) lt))
    (_       (error `look-key->view "bad look key"))))

(current-pict3d-fov 60)
