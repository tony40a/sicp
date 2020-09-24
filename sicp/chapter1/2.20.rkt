#lang sicp
(define (same-parity x . y)
 (define (even-odd y r)
      (if (null? y)
          nil
          (if (= (remainder (car y) 2) r )
              (cons (car y) (even-odd (cdr y) r))
              (even-odd (cdr y) r))))
     (cons x (even-odd y (remainder x 2))))


(same-parity 11 2 3 4 5 6 7)
        