#lang sicp


;аккумулятор последовательности
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;процедура формирования списка элементов от low к high
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (sequence_simple_n n);все пары i,j до n
  (accumulate append
              nil
              (map (lambda (i) ;внешний цикл
                     (map (lambda (j) (list i j)) ;как бы внутренний цикл
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(sequence_simple_n 6)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))