#lang sicp
(define (square x) (* x x))
(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (test? a n)
  (= (expmod a n n) a))
(define (test-all n)
  (test-all-from 1 n))
(define (test-all-from start n)
  (if (< start n)
     (if (test? start n)
         (test-all-from (+ start 1) n)
         false)
     true))

(test-all 561);a^561=a(mod 561) a < 561
(test-all 1105)
(test-all 1729)
(test-all 2465)
(test-all 2821)
(test-all 6601)