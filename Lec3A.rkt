#lang racket

(define (+vect v1 v2)
  (make-vector
   (+ (xcor v1) (xcor v2))
   (+ (ycor v1) (ycor v2))))


(define (scale s v)
  (make-vector (* s (xcor v))
               (* s (ycor v))))


(define make-vector cons) ; procedure likes object can be named
(define xcor car)
(define ycor cdr)

; Representing Line Segments
(define make-seg cons)
(define seg-start car)
(define seg-end cdr)

(make-seg (make-vector 2 3)
          (make-vector 5 1))

(cons 1 (cons 2 (cons 3 (cons 4 null)))) ; list to glue sequence of things together
(list 1 2 3 4)  ; syntatic sugar to above

(define 1-to-4 (list 1 2 3 4))
(car (cdr 1-to-4)) ; evaluate 2
(car (cdr (cdr 1-to-4))) ; 3

(define (scale-list mult lis)
  (if (null? lis)
      null
      (cons (* (car lis) mult)
            (scale-list mult (cdr lis)))))

(scale-list 10 1-to-4)

; Intro to map
(define (map proc lis)
  (if (null? lis)
      null
      (cons (proc (car lis))
            (map proc (cdr lis)))))


(define (scale-list2 s l)
  (map (lambda (item) (* item s))
       l))

(scale-list2 5 1-to-4) ; '(5 10 15 20)

(define (square x) (* x x))
(map square 1-to-4) ; '(1 4 9 16)
(map (lambda (x) (+ x 10)) 1-to-4) ; take the lambda function apply to every element in the list: '(11 12 13 14)

(define (for-each proc lis)
  (cond ((null? lis) "done")
        (else (proc (car lis)) ; proc do it to the first element
              (for-each proc  ; do it to the rest of the list
                        (cdr lis)))))


; Primitives, Means of Combination, Means of Abstraction
; Peter Henderson

; Let say we have these functions: make-rect, horiz, vert, origin
; Transform one picture into another

; (x,y) -> origin + x.horiz + y.vert

(define (coord-map rect)
  (lambda (point)
    (+vect
     (+vect (scale (xcor point)
                   (horiz rect))
            (scale (ycor point)
                   (vert rect)))
     (origin rect))))


; Constructing Primitive Pictures from Lists of Segments
(define (make-picture seglist)
  (lambda (rect)
    (for-each
     (lambda (s)
       (drawline
        ((coord-map rect) (seg-start s))
        ((coord-map rect) (seg-end s))))
     seglist)))

(define R (make-rect ...))

(define G (make-picture ... ))

























