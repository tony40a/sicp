#lang sicp
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (get-origin frame)
  (car frame))

(define (get-edge1 frame)
  (cadr frame))

(define (get-edge2 frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (get-origin frame)
  (car frame))

(define (get-edge1 frame)
  (cadr frame))

(define (get-edge2 frame)
  (cddr frame))