#lang racket

(require pict3d)
(provide look-right look-up roll-right angle->rad)
; goal:

; w - move forward
; e - roll right
; q - roll left
; up/down/left/right are just looking around

; we want to track the current position, current forward vector, current up vector, and go from there

; given a linear transformation of form (linear right forward up)
; be able to look up, look right, or roll some theta

(define (angle->rad ang)
  (* ang (/ pi 180)))

(define (look-right theta lt)
  (match-define (linear R1 F1 U) lt)
  (define F2
    (dir+ (dir-scale F1 (cos theta))
          (dir-scale R1 (sin theta))))
  (define R2 (dir-cross F2 U))
  (linear R2 F2 U))

#;((look-right identity-linear) (/ pi 6))

(define (look-up theta lt)
  (match-define (linear R F1 U1) lt)
  (define F2
    (dir+ (dir-scale F1 (cos theta))
          (dir-scale U1 (sin theta))))
  (define U2 (dir-cross R F2))
  (linear R F2 U2))

#;((look-up identity-linear) (/ pi 6))

(define (roll-right theta lt)
  (match-define (linear R1 F U1) lt)
  (define R2
    (dir- (dir-scale R1 (cos theta))
          (dir-scale U1 (sin theta))))
  (define U2 (dir-cross R2 F))
  (linear R2 F U2))

#;((roll-right identity-linear) (/ pi 6))
