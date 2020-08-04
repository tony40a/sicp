#lang sicp

(define (square x) (* x x))
(define (sumsq a b c)
  (cond ((and (>= a b) (> b c)) (+ (square a) (square b)))
        ((> a b) (+ (square a) (square c)))
        ((+ (square b) (square c))) ))
(sumsq 4 5 4)