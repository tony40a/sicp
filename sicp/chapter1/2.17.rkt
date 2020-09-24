#lang sicp

(define (last-pair-op list); оптимальный вариант
  (if (null? (cdr list)) 
      list 
      (last-pair-op (cdr list))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
(length-iter items 0))

(define (last-pair list)
  (list-ref list (- (length list) 1)))


(last-pair (list 23 72 149 34))
