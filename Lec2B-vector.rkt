#lang racket

; representing vectors in the plane

(define (make-vector x y) (cons x y))

(define (xcor p) (car p))
(define (ycor p) (cdr p))

; representing line segments
(define (make-seg p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

(define (midpoints s)
  (let ((a (seg-start s))
        (b (seg-end s)))
    (make-vector
     (average (xcor a) (xcor b))
     (average (ycor a) (ycor b)))))

(define (average a b)
  (/ (+ a b) 2))

; pythagorus theorem
(define (length s)
  (let
      ((dx (- (xcor (seg-end s))
              (xcor (seg-start s))))
       (dy (- (ycor (seg-end s))
              (ycor (seg-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))

(define (square a)
  (* a a))

(define p (make-vector 1 2))
(define q (make-vector 2 3))
(xcor p)
