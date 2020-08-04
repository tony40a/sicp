#lang sicp
(define (A x y)
(cond ((= y 0) 0)
((= x 0) (* 2 y))
((= y 1) 2)
(else (A (- x 1)
(A x (- y 1))))))

(A 1 10);2^10
(A 2 4);2^2^2^2 (4 times)
(A 3 3);2^2^2^2