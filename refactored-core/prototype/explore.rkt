#lang typed/racket

(provide explore
         FPS
         DELTA-LOOK
         DELTA-MOVE)

(require pict3d
         pict3d/universe)



(define FPS
  (ann
   (make-parameter (ann 144 Positive-Integer))
   (Parameterof Positive-Integer)))

(define DELTA-LOOK
  (ann
   (make-parameter (ann (degrees->radians .5) Real))
   (Parameterof Real)))


(define DELTA-MOVE
  (ann (make-parameter (/ 20 (FPS)))
       (Parameterof Positive-Real)))

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
              #:frame-delay (ann (ceiling (/ 1000 (FPS))) Positive-Integer)
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
      (pos+ acc (move-key->dir ws.view key) (DELTA-MOVE))))
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
    ("e"     (roll-right (DELTA-LOOK) lt))
    ("right" (look-right (DELTA-LOOK) lt))
    ("up"    (look-up (DELTA-LOOK) lt))
    ("q"     (roll-right (- (DELTA-LOOK)) lt))
    ("left"  (look-right (- (DELTA-LOOK)) lt))
    ("down"  (look-up (- (DELTA-LOOK)) lt))
    (_       (error `look-key->view "bad look key"))))


(: look-right (-> Real Linear Linear))
(define (look-right theta lt)
  (match-define (linear R1 F1 U) lt)
  (define F2
    (dir+ (dir-scale F1 (cos theta))
          (dir-scale R1 (sin theta))))
  (define R2 (dir-cross F2 U))
  (linear R2 F2 U))

(: look-up (-> Real Linear Linear))
(define (look-up theta lt)
  (match-define (linear R F1 U1) lt)
  (define F2
    (dir+ (dir-scale F1 (cos theta))
          (dir-scale U1 (sin theta))))
  (define U2 (dir-cross R F2))
  (linear R F2 U2))

(: roll-right (-> Real Linear Linear))
(define (roll-right theta lt)
  (match-define (linear R1 F U1) lt)
  (define R2
    (dir- (dir-scale R1 (cos theta))
          (dir-scale U1 (sin theta))))
  (define U2 (dir-cross R2 F))
  (linear R2 F U2))
