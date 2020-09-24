#lang sicp
(define (reverse items)
  (define (reverse-iter source result) 
    (if (null? source) 
        result 
        (reverse-iter (cdr source) (cons (car source) result)))) 
  (reverse-iter items (list)))

(define (deep-reverse items)
  (define (deep-reverse-iter source result)
    (if (null? source)
        result
        (deep-reverse-iter (cdr source) (cons (reverse (car source)) result))))  
    (deep-reverse-iter items (list)))

(define (deep-reverse2 items) 
  (define (deep-reverse-iter source result) 
    (define (reverse-it element) ;используем deep-reverse процедуру повторно к самой себе 
      (if (list? element) 
          (deep-reverse2 element) 
          element)) 
    (if (null? source) 
        result 
        (deep-reverse-iter (cdr source) 
                           (cons (reverse-it (car source)) result)))) 
  (deep-reverse-iter items (list)))

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)