#lang racket

(define (+Rat x y)
  (Make-Rat
   (+ (* (Numer x) (Denom y))
      (* (Numer y) (Denom x)))
   (* (Denom x) (Denom y))))
   

(define (*Rat x y)
  (Make-Rat
   (* (Numer x) (Numer y))
   (* (Denom x) (Denom y))))

;(define (Make-Rat N D)
;  (cons N D))

(define (Make-Rat n d)
    (let ((g (gcd n d))) ; let to establish local name
        (cons (/ n g) ; divide to lowest term
            (/ d g))))

(define (Numer x) (car x))

(define (Denom x) (cdr x))

(define A (Make-Rat 1 2))

(define B (Make-Rat 1 4))

(define Ans (+Rat A B))

(Numer Ans)
(Denom Ans)









