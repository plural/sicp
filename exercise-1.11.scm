#lang planet neil/sicp

; Exercise 1.11 - A function f is defined by the rule that
;   f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.
; Write a procedure that computes f by means of a recursive process.
; Write a procedure that computes f by means of an iterative process.

(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (dec n))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3)))))))

(define (f-iterative n)
  (cond ((< n 3) n)
        (else (f-iter n 3 2 1 0))))

(define (f-iter limit i x y z)
  (cond ((= limit i) (+ x (* 2 y) (* 3 z)))
        (else (f-iter limit (inc i) (+ x (* 2 y) (* 3 z)) x y))))
