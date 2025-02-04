#lang racket

(require pict3d
        pict3d/universe)
(current-pict3d-legacy? #t)
(struct ws (scene pos view keyset) #:transparent)

; yaw - around z
; pitch - around x
(struct view (yaw pitch) #:transparent)

(define INIT-SCENE (combine (cube origin 1)
                            (light (pos 2 0 0) (emitted "red"))
                            (light (pos 0 2 0) (emitted "blue"))
                            (light (pos 0 0 2) (emitted "green"))))
                            
(define INIT-POS (pos 5 -5 0))
(define INIT-VIEW (view 135 0))

(define INIT-KEYSET (set))

(define MOVE-KEYSET (set "w" "a" "s" "d" " " "shift"))

(define LOOK-KEYSET (set "left" "right" "up" "down"))

#;(combine INIT-SCENE (basis 'camera (point-at INIT-POS
                                               (angles->dir 135 30)
                                               )))

(define INIT-WS (ws INIT-SCENE INIT-POS INIT-VIEW INIT-KEYSET))

(define (on-draw state n t)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (match-define (view view.yaw view.pitch) ws.view)
  (combine ws.scene
           (basis 'camera (point-at ws.pos (angles->dir view.yaw view.pitch)))))

(define (on-key state n t k)
  (match-define (ws ws.scene ws.pos ws.view ws.keys) state)
  (match k
    ((or "w" "a" "s" "d" "shift" " " "left" "right" "up" "down")
     (ws ws.scene ws.pos ws.view (set-add ws.keys k)))
    (_ state)))

(define (on-release state n t k)
  (match-define (ws ws.scene ws.pos ws.view ws.keyset) state)
  (match k
    ((or "w" "a" "s" "d" "shift" " " "left" "right" "up" "down")
     (ws ws.scene ws.pos ws.view (set-remove ws.keyset k)))
    (_ state)))


(define ((move-key->dir dir-look) k)
  (match-define (dir dx dy dz) dir-look)
  (match k
    ("w" (dir dx dy 0))
    ("a" (dir (- dy) dx 0))
    ("s" (dir-negate ((move-key->dir dir-look) "w")))
    ("d" (dir-negate ((move-key->dir dir-look) "a")))
    ("shift" (dir 0 0 -1))
    (" " (dir 0 0 1))
    (_ (error 'key->dir "unknown key"))
    ))

(define LOOK-DELTA 2) ; degrees
(define (look-key->view k v)
  (match-define (view view.yaw view.pitch) v)
  (match k
    ("left" (view (+ view.yaw LOOK-DELTA) view.pitch))
    ("right" (view (- view.yaw LOOK-DELTA) view.pitch))
    ("up" (view view.yaw (+ view.pitch LOOK-DELTA)))
    ("down" (view view.yaw (- view.pitch LOOK-DELTA)))))

(define DELTA-MOVE 0.1)

(define (on-frame state n t)
  (match-define (ws ws.scene ws.pos ws.view ws.keyset) state)
  (match-define (view view.yaw view.pitch) ws.view)
  (define camera-dir (angles->dir view.yaw view.pitch))
  (ws
   ws.scene
   (foldr
    (λ (a-dir acc) (pos+ acc a-dir DELTA-MOVE)) ; func
    ws.pos ; start
    (map (move-key->dir camera-dir) (set->list (set-intersect MOVE-KEYSET ws.keyset)))); list
   (foldr look-key->view ws.view (set->list (set-intersect LOOK-KEYSET ws.keyset)))
   ws.keyset))





(big-bang3d INIT-WS
            #:on-draw on-draw
            #:on-key on-key
            #:on-release on-release
            #:on-frame on-frame)