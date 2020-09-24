#lang sicp
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
(length-iter items 0))

(define (reverse list) ;неоптимальный, сложность n^2
  (define (reverse-list list count)
    (if (< count 0)
     nil
    (cons (list-ref list count) (reverse-list list (- count 1)))))
  (reverse-list list (- (length list) 1)))

(define (reverse items) ;оптимальный, сложность n
  (define (reverse-iter source result) 
    (if (null? source) 
        result 
        (reverse-iter (cdr source) (cons (car source) result)))) 
  (reverse-iter items (list))) ;передаем список


(reverse (list 1 4 9 16 25))