#lang sicp
(define (sqrt-iter guess x)
  (if (good-enough? guess x);predicate
          guess;then-clause
          (sqrt-iter (improve guess x)
                     x);else-clause
                  ))
(define(improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.000001));next one - current
(define (square x) (* x x))
(define (sqrt x) (sqrt-iter 1.0 x))


(sqrt 0.000001)