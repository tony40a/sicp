#lang sicp
(define one-through-four (list 1 2 3 4))

(car (cdr (cdr (cdr one-through-four))))

(define test-list (cons 1 (cons 2 3)))



;(list-ref one-through-four 1)

(define (length items); рекурсивный процесс
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
(length-iter items 0))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items) ;общая процедура обработки списков
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;(map (lambda (x) (* x x))
;     (list 1 2 3 4))

(define (scale-list-map items factor) ;scale-list через map
  (map (lambda (x) (* x factor))
       items))

;(define odds (list 1 3 5 7))

;(length odds)

