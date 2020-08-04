#lang sicp
(define (cont-frac n d k)
  (define (iter i)
    (/ (n i) (+ (d i)
                (if (< i k)
                    (iter (+ i 1))
                    0))))
  (iter 1))



(define (d k)
  (define (d-even i n)
    (if (> i n)
        0
        (+ (d-even (+ i 1) n) 2)
        ))
  (define (iter i j n product)
    (if (> i k)
        product
        (if (or (= i 1) (< j 2))
            (iter (+ i 1) (+ j 1) n 1)
            (iter (+ i 1) 0 (+ n 1) (d-even 0 n)))))
  (iter 1 1 0 1))

;более простой альтернативный вариант, основанный на остатке от деления на 3
(define (e1 k) 
  (+ 2 (cont-frac (lambda (i) 1.0) 
                  (lambda (i) (if (= (remainder i 3) 2) 
                                     (* 2 (/ (+ i 1) 3)) 
                                     1)) 
                  k)))
      
        

;(d 11)
(define (e2 k) 
  (+ 2 (cont-frac (lambda (i) 1.0) d 100)))

(e1 10)
(e2 10)
(e1 100)
(e2 100)