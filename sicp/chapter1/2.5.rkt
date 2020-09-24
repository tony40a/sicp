#lang sicp

(define (power base exponent) 
  (define (power-iter base counter product) 
    (if (= counter 0) 
        product 
        (power-iter base (- counter 1) (* base product)))) 
  (power-iter base exponent 1))

(define (factor base value) 
  (define (factor-iter value counter) 
    (if (= (remainder value base) 0) 
        (factor-iter (/ value base) (+ counter 1)) 
        counter)) 
  (factor-iter value 0))

(define (cons a b) 
  (* (power 2 a) (power 3 b)))

(define (car pair) 
  (factor 2 pair))

(define (car pair) 
  (factor 3 pair))


