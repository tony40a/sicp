#lang sicp
(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iteration)
    (display iteration)
    (display ": ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ iteration 1)))))
  (try first-guess 1))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (f x) (/ (log 1000) (log x)))

(define (g x)  (average x (/ (log 1000) (log x))))

(fixed-point g 2.0)
