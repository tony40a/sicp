#lang sicp
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
(if (not (> (abs angle) 0.1))
angle
(p (sine (/ angle 3.0)))))

(sine 12.15)

;линейно - рекурсивный процесс
;p вызовется 5 раз. (((angle/3)/3)..) = ang/(3^n)
;a/(3^n) <= 0.1  n>= log3(10a) (по осованию 3) O(log a) также растет память