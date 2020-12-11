#lang racket

; Symbolic Differentiation and Quotation
; Quotation ': suspends evaluation

(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var)))
        ((product? exp)
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (deriv (m1 exp) var)
                        (m2 exp)))
         )))

(define foo       ; ax^2 + bx + c
  '(+ (* a (* x x))
      (+ (* b x)
         c)))

(define (atom? x)  ; test x is number, not list or null
  (and (not (null? x))
       (not (pair? x))))

(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define a1 cadr)  ; car of cdr, return second element of the list
(define a2 caddr) ; return third element

; (+ 3 5) = node(+, next) -> node(3, next) -> node(5, null)

(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(define (make-product m1 m2)
  (list '* m1 m2))

(define m1 cadr)
(define m2 caddr)
  
(deriv foo 'x) ; differentiate ax^2 + bx + c wrt x, get 2ax + b
(deriv foo 'a) ; deriv ax^2 + bx + c wrt a, get x^2
(deriv foo 'b) ; deriv ax^2 + bx + c wrt b, get x
(deriv foo 'c) ; deriv ax^2 + bx + c wrt c, get 1


#|
Abstraction layer

===============
Derivation rules
===============
constant?
same-var?
sum?
make-sum
a1
a2
================
Representational list structure
|#

(define (make-sum-simplify a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        (else (list '+ a1 a2))))

(define (make-product-simplify a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (* a1 a2))
        ((and (number? a1) (= a1 0))
         0)
        ((and (number? a2) (= a2 0))
         0)
        ((and (number? a1) (= a1 1))
         a2)
        ((and (number? a2) (= a2 1))
         a1)
        (else (list '* a1 a2))))

(define (deriv-simplify exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum-simplify (deriv-simplify (a1 exp) var)
                   (deriv-simplify (a2 exp) var)))
        ((product? exp)
         (make-sum-simplify
          (make-product-simplify (m1 exp)
                        (deriv-simplify (m2 exp) var))
          (make-product-simplify (deriv-simplify (m1 exp) var)
                        (m2 exp)))
         )))


(deriv-simplify foo 'x) ; differentiate ax^2 + bx + c wrt x, get 2ax + b
(deriv-simplify foo 'a) ; deriv ax^2 + bx + c wrt a, get x^2
(deriv-simplify foo 'b) ; deriv ax^2 + bx + c wrt b, get x
(deriv-simplify foo 'c) ; deriv ax^2 + bx + c wrt c, get 1

; the purpose of inventing quotation so that the result similar to math that we familiar with





