#lang sicp
(define (double x)
  (* x 2))
(define (even? n)
  (= (remainder n 2) 0))

(define (halve x)
  (if (not (even? x)) x (/ x 2)))

(define (fast-mult* a b) 
  (cond ((= b 0) 0) 
        ((= b 1) a) 
        ((even? b) (double (fast-mult* a (halve b)))) 
        (else (+ a (fast-mult* a (- b 1))))))

(fast-mult* 3 3)