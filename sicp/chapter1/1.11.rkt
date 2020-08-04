#lang sicp
(define (fib1 n)
  (cond ((< n 3) n)
        (else (+ (fib1 (- n 1)) (fib1 (- n 2)) (fib1 (- n 3))))))

(define (fib2 n)
  (fib-iter 2 1 0 n))

(define (fib-iter a b c count)
  (if (= count 0)
      c
      (fib-iter (+ a b c) a b (- count 1))))

(fib1 10)
(fib2 10)