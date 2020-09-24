#lang sicp
(define find-seven (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr find-seven)))))

(define find-seven2 (list (list 7)))

(car (car find-seven2))

(define find-seven3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr find-seven3))))))))))))