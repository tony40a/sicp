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

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by interval that spans zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-percent c wp)
  (make-interval (- c (* (/ wp 100) c)) (+ c (* (/ wp 100) c))))

(define (percent1 i)
  ((/ (- (center i) (lower-bound i)) (center i)) * 100))

(define (percent i) ; альтернативный вариант percent = (upper-bound – center) / center * 100 = (upper-bound – (lower-bound + upper-bound)/2) / [(lower-bound + upper-bound)/2] * 100 = (upper-bound – lower-bound) / (upper-bound + lower-bound) * 100
  (let ((l (lower-bound i)) (u (upper-bound i))) 
    (* 100 (/ (- u l) (+ u l)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (print-interval int)
  (newline)
  (display "(")
  (display (lower-bound int))
  (display ",")
  (display (upper-bound int))
  (display ")"))


(define A (make-interval 7 8))
(define B (make-interval 25 28))

(define Ac (make-center-percent 100 1))
(define Bc (make-center-percent 100 5))

(print-interval (par1 Ac Bc))
(print-interval (par2 Ac Bc))

;(print-interval (div-interval A A))
;(print-interval (div-interval Ac Ac))
;(print-interval (div-interval A B))

