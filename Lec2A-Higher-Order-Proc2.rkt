#lang racket

; Main idea of this lecture is to have procedure takes in procedure as argument
; and return another procedure

(define (average x y)
  (* (+ x y) 0.5))

(define (sqrt x)
  (define tolerance 0.0001)
  (define (good-enuf? y)
    (< (abs (- (* y y) x)) tolerance))
  (define (improve y)
    (average (/ x y) y))
  (define (try y)
    (if (good-enuf? y)
        y
        (try (improve y))))
  (try 1))


; By wishful thinking, let say there is function fixed-point
(define (sqrt2 x)
  (fixed-point
   (lambda (y) (average (/ x y) y))
   1))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))


(define (sqrt3 x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))  ; take proc as argument and return proc
   1))


; this is same as writing like this
; (define (average-damp f)
;   (lambda (x) (average (f x) x)))
(define average-damp
  (lambda (f)  ; a proc takes in proc to produce proc
    (lambda (x) (average (f x) x))))






