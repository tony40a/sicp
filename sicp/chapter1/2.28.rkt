#lang sicp
(define (fringe x)
  (define (fringe-rev items)
    (cond ((null? items) (list))
          ((pair? items) (append (fringe-rev (car items)) (fringe-rev (cdr items))))
          (else (list items)))) ;здесь важно формировать из элемента list, тогда append сработает
  (fringe-rev x))



(define x (list (list 1 2) (list 3 4)))

(cadr x)