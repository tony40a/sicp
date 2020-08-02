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

(define (cube x) (* x x))

(define (square x) (* x x))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess 0.00001))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                    1.0))



(sqrt 4)