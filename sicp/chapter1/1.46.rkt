#lang sicp
(define (iterative-improve check? improve)
   (define (try guess) 
    (let ((next (improve guess))) 
         (if (check? guess next) 
             next 
             (try next)))) 
  try)

(define (fixed-point f first-guess) 
  ((iterative-improve (lambda (guess next) 
                              (< (abs (- guess next)) 
                                 tolerance)) 
                      f) first-guess))

(define (sqrt x) 
  ((iterative-improve (lambda (guess next) 
                              (< (abs (- (square guess) x)) 
                                 tolerance)) 
                      (lambda (guess) 
                              (average guess (/ x guess)))) 1.0))