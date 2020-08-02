#lang sicp
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
  
(define (cube x) (* x x x))

(define (f x) x)

(define (inc n) (+ n 1))

(define (sum a b)
  (accumulate-iter + 0 cube a inc b))

(define (product a b)
  (accumulate-iter * 1 f a inc b))

(sum 2 3)

(define (factorial n)
  (product 1 n))

(factorial 5)