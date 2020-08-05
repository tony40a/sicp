#lang sicp
(define (square x) (* x x))

;способа задания прямоугольника два: 1) с помощью двух точек 2) с помощью точки, ширины и длины прямоугольника

(define (make-rect width length); это не совсем верно поскольку два отрезка могут и не пересекаться или пересекаться не в краях, тут нужна левая верхняя точка для задания отсчета сторон
  (cons width length))

(define (rect-w rect)
  (car rect))

(define (rect-l rect)
  (cdr rect))

(define (segment-length rect-side)          
    (let ( (x1 (x-point (start-segment (rect-side rect))))
           (y1 (y-point (start-segment (rect-side rect))))
           (x2 (x-point (end-segment (rect-side rect))))
           (y2 (y-point (end-segment (rect-side rect)))) )
      ( sqrt (+ (square (- x1 x2)) (square (- y1 y2))) )))

(define (square-r rect)
  (* (segment-length rect-w) (segment-length rect-l)))

(define (perim-r recr)
  (* (+ (segment-length rect-w) (segment-length rect-l) ) 2))

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

(define rect (make-rect (make-segment (make-point 2 4) (make-point 2 1)) (make-segment (make-point 2 4) (make-point 8 4))))

(square-r rect)

(perim-r rect)


