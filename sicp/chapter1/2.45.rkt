#lang sicp
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (split main-operation sub-operation) 
  (define (generic-split painter n) 
    (if (= n 0) 
        painter 
        (let ((smaller (generic-split painter (- n 1)))) 
          (main-operation painter (sub-operation smaller smaller))))) 
  generic-split)

(define right-split (split beside below))
(define up-split (split below beside))