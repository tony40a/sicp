#lang sicp
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define one-third (cons 1 3))

(car one-third)

(cdr one-third)