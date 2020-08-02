#lang sicp
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (f x) x)

(define (inc n) (+ n 1))

(define (fact n)
  (product-iter f 1 inc n))

(define (jwf n)
  (define (term k)
    (if (even? k) (/ k (+ k 1))
        (/ (+ k 1) k)))
  (* 4.0 (product-iter term 2 inc n)))

(jwf 1000)