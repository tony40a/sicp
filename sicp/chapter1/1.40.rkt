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

(define dx 0.00001)

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess 0.0000000001))

(define (cubic a b c)
   (lambda(x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (cubic-f a b c)
  (newtons-method (cubic a b c) 1))

(cubic-f 1 -4 -4)