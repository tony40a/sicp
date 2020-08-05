#lang sicp
(define (make-segment start-p end-p)
  (cons start-p end-p))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment segment)
  (let ((x1 (x-point (start-segment segment)))
    (x2 (x-point (end-segment segment)))
    (y1 (y-point (start-segment segment)))
    (y2 (y-point (end-segment segment))))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define line1 (make-segment (make-point 4 2) (make-point 6 12)))

(print-point (midpoint-segment line1))


