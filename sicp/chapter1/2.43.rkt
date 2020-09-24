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
  (accumulate append nil (map proc seq))) ;последовательность порожденная применением proc к seq

;процедура формирования списка элементов от low к high
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;тест safe
(define (safe-test? k positions)
  true)

;пустая доска
(define empty-board (list))

;добавление ферзя на новую строку
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

;является ли позиция безопасной для ферзя (не попадает под удар других ферзей по горизонтали и диагонали, вертикаль обеспечивается позицией в списке)
(define (safe? k position)
  (define (queens-safe? queen-count rest-rows)
    (define (queen-safe? col row)
      (let ((last-col k)
            (last-row (car position)))
        (and (not (= last-row row))
             (not (= (abs (- last-row row))
                     (abs (- last-col col)))))));
    (cond ((null? rest-rows) true)
          ((queen-safe? queen-count (car rest-rows))
           (queens-safe? (- queen-count 1) (cdr rest-rows)))
          (else false)));
  (queens-safe? (- k 1) (cdr position)))
      
;возвращает возможные расстановки ферзей на доске размерности board-size
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe-test? k positions))
         (flatmap ;генерирует список списков
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(queens 4)


;T(8) = P * 8 * (L(7) + L(6) + ... + L(1)) = P * 8 * 56 = 448*P, P-время на расстановку ферзя, L-количество рещений для N

;Th(8) = P * 8 * (L(7) + 8*L(6) + ... +86*L(1)) = P * 8 * (40+32+640+1024) = 13888*P.
