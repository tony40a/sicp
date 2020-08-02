#lang sicp
(define (square x) (* x x))

(define (inc x) (+ 1 x))

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1))))) 

((repeated square 2) 5)

((repeated inc 3) 5)

