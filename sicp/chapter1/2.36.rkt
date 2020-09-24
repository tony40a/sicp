#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))


(define (accumulate-n op init seqs)
  (cond ((null? seqs) nil) ;без этого выдает ошибку mcar contract violation, невозможно проверить на пустоту (car seqs) если seqs пустой
        ((null? (car seqs)) nil)
        (else (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs))))))

;складывает первые элементы списков списка
(accumulate-n + 0 s)
