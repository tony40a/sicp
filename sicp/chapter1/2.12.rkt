#lang sicp
(define (make-interval a b) (cons a b))

(define (lower-bound int)
  (car int))

(define (upper-bound int)
  (cdr int))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c wp)
  (make-interval (- c (* (/ wp 100) c)) (+ c (* (/ wp 100) c))))

(define (percent1 i)
  ((/ (- (center i) (lower-bound i)) (center i)) * 100))

(define (percent i) ; альтернативный вариант percent = (upper-bound – center) / center * 100 = (upper-bound – (lower-bound + upper-bound)/2) / [(lower-bound + upper-bound)/2] * 100 = (upper-bound – lower-bound) / (upper-bound + lower-bound) * 100
  (let ((l (lower-bound i)) (u (upper-bound i))) 
    (* 100 (/ (- u l) (+ u l)))))