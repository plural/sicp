#lang planet neil/sicp

; Exercise 1.15.  The sine of an angle (specified in radians) can be computed
; by making use of the approximation sin x ~ x if x is sufficiently small,
; and the trigonometric identity
; sin r = 3*sin(r/3) - 4 sin^3 (r/3)

; to reduce the size of the argument of sin. (For purposes of this exercise an
; angle is considered "sufficiently small" if its magnitude is not greater than
; 0.1 radians.) These ideas are incorporated in the following procedures:

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))
