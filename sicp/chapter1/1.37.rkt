#lang sicp
(define (fixed-point f first-guess tolerance) 
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)) 
  (define (try guess) 
    (let ((next (f guess))) 
         (if (close-enough? guess next) 
             next 
             (try next)))) 
  (try first-guess))

(define (cont-frac n d k)
  (define (iter i)
    (/ (n i) (+ (d i)
                (if (< i k)
                    (iter (+ i 1))
                    0))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter i product)
    (if (> i k)
        product
        (iter (+ i 1) (/ (n i) (+ (d i) product)))))
  (iter 1 0))

(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100)) 