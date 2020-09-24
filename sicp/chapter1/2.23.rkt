#lang sicp
(define (for-each proc items)
  (cond ((not (null? items)) ;здесь если false останавливаемся (not (true)) - условие сито
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))