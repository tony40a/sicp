#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (cond ((null? seqs) nil) ;без этого выдает ошибку mcar contract violation, невозможно проверить на пустоту (car seqs) если seqs пустой
        ((null? (car seqs)) nil)
        (else (cons (accumulate op init (map (lambda (x) (car x)) seqs))
                    (accumulate-n op init (map (lambda (x) (cdr x)) seqs))))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define vector (list 4 3 1 2))

(define vector2 (list 5 2 7 1))

;умножение матрицы на вектор (первая строка матрицы на вектор, вторая строка на вектор и т.д.)
(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (map * v x))) m));или (dot-product v w) что тоже самое

(matrix-*-vector matrix vector)

;транспозиция матрицы
(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define matrix1 (list (list 1 2 1) (list 0 1 2)))

(define matrix2 (list (list 1 0) (list 0 1) (list 1 1)))

(matrix-*-matrix matrix1 matrix2)

  