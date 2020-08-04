#lang sicp
(define dx 0.5)

(define (square x) (* x x))

(define (average f)
  (lambda (x)(/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1))))) 

;(define (smooth f n)
;  (repeated (average f) n))

(define (smooth f n)
  ((repeated (lambda (g) (average g)) n) f))

((smooth square 2) 1.0)


  