(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (sum-int a b)
  (define (identity a) a)
  (sum identity a inc b))


(define (square x)
  (* x x))

(define (sum-of-squares a b)
  (sum square a inc b))

(define (pi-sum a b)
  (sum (lambda(i) (/ 1 (* i (+ i 2))))
       a
       (lambda(i) (+ i 4))
       b))

(define (iter-sum term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a 0))

(define (sqrt x)
  (define tolerance 0.00001)
  (define (good-enuf? y)
    (< (abs (- (* y y) x)) tolerance))
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve y)
    (average (/ x y) y))
  (define (try y)
    (if (good-enuf? y)
        y
        (try (improve y))))
  (try 1))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (fp-sqrt x)
  (fixed-point
   (average-damp (lambda(y) (/ x y)))
   1))

(define (average x y)
  (/ (+ x y) 2))

; average-damp will take a procedure as an argument and return a procedure as its value
(define average-damp
  (lambda(f)
    (lambda(x) (average (f x) x))))

(define (newton-sqrt x)
  (newton (lambda(y) (- x (square y)))
          1))

(define dx .00000001)

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define (newton f guess)
  (define derivative-f (deriv f))
  (fixed-point
     (lambda(x) (- x (/ (f x) (derivative-f x))))
     guess))
  