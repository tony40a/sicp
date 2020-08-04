#lang sicp
(define (cube x) (* x x x))

(define (square x) (* x x))

(define (deg x n) ;медленный вариант возведения в степень
  (if (= n 0)
      1
      (* x (deg x (- n 1)))))

(define (power x n)
  (exp (* n (log x))))

(define tolerance 0.000000001)

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (fixed-point f first-guess tolerance) 
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)) 
  (define (try guess) 
    (let ((next (f guess))) 
         (if (close-enough? guess next) 
             next 
             (try next)))) 
  (try first-guess))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess tolerance))

(define (log2 x) 
  (/ (log x) (log 2)))

(define (sqrt x power-n)
  (define damp-n (floor (log2 power-n))) 
  (fixed-point-of-transform (lambda (y) (/ x (power y (- power-n 1))))
                            (repeated average-damp damp-n)
                            1.0))

(sqrt 256 8)

;(log 2.7)