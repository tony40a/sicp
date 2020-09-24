#lang sicp
(define (make-interval a b) (cons a b))

(define (upper-bound int)
  (car int))

(define (lower-bound int)
  (cdr int))

(define (sub-interval x y) 
  (make-interval (- (lower-bound x) (upper-bound y)) ;для нижней границы минимизируем уменьшаемое, максимизируем вычитаемое 
                 (- (upper-bound x) (lower-bound y)))) ;для верхней максимизируем уменьшаемое, минимизируем вычитаемое