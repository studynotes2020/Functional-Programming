#lang racket

; Main idea of this lecture is to have procedure takes in procedure as argument

; sum of i from i=a to i=b
(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (add1 a) b))))

(define (sq x)
  (* x x))

; sum of i^2 from i=a to i=b
(define (sum-sq a b)
  (if (> a b)
      0
      (+ (sq a)
         (sum-sq (add1 a) b))))


; Leibniz formula sum of 1/(i*(i+2)) from i=a to i=b
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

;(define (<name> a b)
;  (if (> a b)
;      0
;      (+ (<term> a)
;         (<name> (<next> a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term  ; each line is an argument, this is the first argument
              (next a)  ; apply next procedure () to a
              next  ; pass in next procedure as argument
              b))))


(define (sum-int2 a b)
  (define (identity a) a)
  (sum identity a add1 b))

(define (sum-sq2 a b)
  (sum sq a add1 b))

(define (pi-sum2 a b)
  (sum (lambda (i) (/ 1 (* i (+ 1 2))))
       a
       (lambda (i) (+ i 4))
       b))


; Iterative implemetation of sum
(define (iter-sum term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a 0))

