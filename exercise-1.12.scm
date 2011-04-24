#lang planet neil/sicp
; Exercise 1.12.  The following pattern of numbers is called Pascal's triangle.
; 1                    (1,1)
; 1 1                  (2,1) (2,2)
; 1 2  1               (3,1) (3,2) (3,3)
; 1 3  3  1            (4,1) (4,2) (4,3) (4,4)
; 1 4  6  4  1         (5,1) (5,2) (5,3) (5,4) (5,5)
; 1 5 10 10  5  1      (6,1) (6,2) (6,3) (6,4) (6,5) (6,6)
; 1 6 15 20 15  6 1    (7,1) (7,2) (7,3) (7,4) (7,5) (7,6) (7,7)
; 1 7 21 35 35 21 7 1  (8,1) (8,2) (8,3) (8,4) (8,5) (8,6) (8,7) (8,8)
; The numbers at the edge of the triangle are all 1, and each number inside the
; triangle is the sum of the two numbers above it. Write a procedure that
; computes elements of Pascal's triangle by means of a recursive process.

(define (pascals-triangle row col)
  (cond ((or (= col 1) (= row col)) 1)
        ((or (= col 2) (= (dec row) col)) (dec row))
        (else (+
               (pascals-triangle (dec row) (dec col))
               (pascals-triangle (dec row) col)))))

; TODO(jgessner): write the iterative version of this function as well.
; The iterative version of this function requires lists, which i don't yet know
; how to do in scheme.  :)
