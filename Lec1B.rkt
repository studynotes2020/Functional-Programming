#lang racket

(define (Sos x y)
  (+ (sq x) (sq y)))

(define (sq x)
  (* x x))



; recursion, not working, as racket doesn't have primitive -1+, 1+
; Peano axioms
;(define (+ x y)
;  (if (= x 0)
;      y
;      (+ (-1+ x) (1+ y))))

; Iteration
;(define (+ x y)
;  (if (= x 0)
;      y
;      (1+ (+ (-1+ x) y))))


; recursion, time O(fib(n)), space O(x)
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (move n from to spare)
  (cond ((= n 0) "Done")
        (else
         (move (sub1 n) from spare to)
         (print-move from to)
         (move (sub1 n) spare to from))))

(define (print-move x y)
  (printf "move from ~a to ~a\n" x y))
