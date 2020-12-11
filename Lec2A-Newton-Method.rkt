#lang racket

(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1))


; To find y such that f(y) = 0
; Start with a guess, y0
; yn+1 = yn - f(yn)/ f'(yn)
(define (newton f guess)
  (define df (deriv f))
  (fixed-point
   (lambda (x) (- x (/ (f x) (df x))))  ; equation of Newton method
   guess))

 
(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))


(define dx 0.00001)
(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))  ; definition of derivative: (f(x+dx)-f(x))/dx
            (f x))
            dx))))

  
(define (square x)
  (* x x))

 



#|
The rights and privileges of first-class citizens

To be named by variables
To be passed as arguments to procedures
To be returned as values of procedures
To be incorporated into data structures


|#

