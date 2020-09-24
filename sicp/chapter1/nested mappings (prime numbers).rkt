#lang sicp
;найти все такие упорядоченные пары различных целых чисел i и j, где 1 ≤ j < i ≤ n, что i+j является простым

;определение простого числа
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n);Если d — делитель n, то n/d тоже. Но d и n/d не могут оба быть больше √n.
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

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

;процедура формирования списка элементов от low к high
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;генерация последовательности пар чисел до n
(define (sequence_simple_n n);все пары i,j до n
  (accumulate append
              nil
              (map (lambda (i) ;внешний цикл
                     (map (lambda (j) (list i j)) ;как бы внутренний цикл
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(sequence_simple_n 6)

;абстракция для генерации последовательностей
(define (flatmap proc seq)
  (accumulate append nil (map proc seq))) ;последовательность порожденная применением proc к seq

;является ли пара простым числом
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

;последовательность результатов
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;последовательность пар ≤ n, которые в сумме дают простое число 
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

;удаление элемента из множества
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;все возможные перестановки множества
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
                 s)))


(permutations (list 1 2 3 4))

                        





