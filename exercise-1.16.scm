#lang planet neil/sicp

; Exercise 1.16
; Exercise 1.16.  Design a procedure that evolves an iterative exponentiation
; process that uses successive squaring and uses a logarithmic number of steps,
; as does fast-expt. (Hint: Using the observation that (b^n/2)^2 = (b^2)^n/2,
; keep, along with the exponent n and the base b, an additional state variable
; a, and define the state transformation in such a way that the product a*b^n is
; unchanged from state to state. At the beginning of the process a is taken to
; be 1, and the answer is given by the value of a at the end of the process. In
; general, the technique of defining an invariant quantity that remains
; unchanged from state to state is a powerful way to think about the design
; of iterative algorithms.)

(define (exponent-recursive x n)
  (if (= n 0)
      1
      (* x (exponent-recursive x (- n 1)))))

(define (exponent-iterative x n)
  (exp-iter x n 1))

(define (exp-iter x counter product)
  (if (= counter 0)
      product
      (exp-iter x
                (- counter 1)
                (* x product))))

(define (square x)
  (* x x))

(define (fast-exp x n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp x (/ n 2))))
        (else (* x (fast-exp x (- n 1))))))

; solution
(define (super-fast-exp x n)
  (if (even? n)
      (square (super-fast-exp-iter x (/ n 2) 1))
      (* x (square (super-fast-exp-iter x (/ (- n 1) 2) 1)))))

(define (super-fast-exp-iter x counter product)
  (if (= counter 0)
      product
      (super-fast-exp-iter x
                           (- counter 1)
                           (* x product))))

; x^4 = square(x^2)
; x^5 = x*square(x^2)
; (define (exp-iter x counter product)
;  (if (= counter 0)
;      product
;      (exp-iter x
;                (- counter 1)
;                (* x product))))
