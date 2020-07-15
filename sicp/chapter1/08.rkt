#lang sicp
(define (square x) (* x x))
(define (qube x) (* x x x))
(define (sqrt3-iter guess x)
  (if (good-enough? guess x);predicate
          guess;then-clause
          (sqrt3-iter (improve guess x)
                     x);else-clause
                  ))
(define(improve guess x)
  (average (/ x (square guess)) (* 2 guess)))
(define (average x y)
  (/ (+ x y) 3))
(define (good-enough? guess x)
  (< (abs (- (qube guess) x)) 0.001))
(define (sqrt3 x) (sqrt3-iter 1.0 x))

(sqrt3 72)