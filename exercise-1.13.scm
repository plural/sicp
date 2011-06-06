#lang planet neil/sicp

; TODO(jgessner): talk to paul and matthias about this proof

; ϕ = phi
; ψ = psi
; Prove that Fib(n) is the closest integer to ϕ^n/√5,
; where ϕ = (1 + √5)/2 == 1.618033988749895
; Hint: Let ψ  = (1 - √5)/2 == -0.618033988749895
; Use induction and the definition of the Fibonacci numbers (see section 1.2.2)
; to prove that Fib(n) = (ϕ^n - ψ^n)/√5.

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (proof n)
  (/ (- (expt 1.618033988749895 n) (expt -0.618033988749895 n)) (sqrt 5)))

; > (fib 2)
; 1
; > (proof 2)
; 0.9999999999999998
; > (fib 3)
; 2
; > (proof 3)
; 2.0
; > (fib 4)
; 3
; > (proof 4)
; 3.0
; > (fib 10)
; 55
; > (proof 10)
; 54.99999999999999
; > (fib 111)
; 70492524767089125814114
; > (proof 111)
; 7.04925247670891e+22
; > (fib 75)
; 2111485077978050
; > (proof 75)
; 2111485077978049.5
