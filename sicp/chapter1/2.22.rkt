#lang sicp
(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) ;здесь происходит сдвиг вправо
                    answer))))
  (iter items nil))

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things)))))) ;здесь cons работает неверно, делает первым элементом nil, затем добавляет упакованные значения списка
  (iter items nil))

(square-list2 (list 1 2 3 4))

(for-each (lambda (x) (newline) (display (square x))) (list 1 2 3 4)) ;for-each применет процедуру к каждому элементу но формирует сам список