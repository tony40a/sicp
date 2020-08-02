#lang sicp
(define (filtered-accumulate1 combiner null-value term a next b filter);нерабочий filter, не учитывается первое число из ряда
  (define (new-next a)
    (if (or (filter (next a)) (> a b))
        (next a)
        (new-next (next a))))
   (if (> a b)
      null-value
      (combiner (term a) (filtered-accumulate combiner null-value term (new-next a) next b filter))))

(define (filtered-accumulate2 combiner null-value term a next b filter);альтернативный вариант
  (if (> a b) 
      null-value 
      (if (filter a) 
          (combiner (term a) 
                    (filtered-accumulate combiner null-value term (next a) next b filter)) 
          (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (filter-check a b)
    (if (filter a b) a null-value))
   (if (> a b)
      null-value
      (combiner (term (filter-check a b)) (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (smallest-divisor n)
  (find-divisor n 2))
  
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n i)
   (= n (smallest-divisor n)))

(define (square x) (* x x))

(define (inc n) (+ 1 n))

(define (sum-square-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (f x) x)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prime-n? i n)
  (if (= (gcd i n) 1)
      true
      false))

(define (plus-num i n)
  (filtered-accumulate * 1 f i inc n prime-n?))



;(prime? 9)
;(sum-square-prime 2 11)
(plus-num 2 12)

