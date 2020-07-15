#lang sicp
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x);predicate
          guess;then-clause
          (sqrt-iter (improve guess x)
                     x);else-clause
                  ))
(define(improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))
(define (sqrt x) (sqrt-iter 1.0 x))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;(sqrt 9)
(define (x) (x))
(new-if #f 3.0 1)