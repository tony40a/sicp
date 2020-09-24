#lang sicp
;аккумулятор последовательности
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;фильтр последовательности
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;абстракция для генерации последовательностей
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;процедура формирования списка элементов от low к high
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;уникальные пары троек
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                       (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1)) ))
           (enumerate-interval 1 n)))

;проверка, является ли сумма трех чисел равной s
(define (equals? triple s)
  (= s (+ (car triple) (cadr triple) (caddr triple)) ))

(define (make-sum-triples triple)
  (list (car triple) (cadr triple) (caddr triple) (+ (car triple) (cadr triple) (caddr triple)))) 

(define (triples-sum n s)
  (map make-sum-triples
       (filter (lambda (x) (equals? x s)) (unique-triples n))))

(triples-sum 6 13)

(unique-triples 6)
        