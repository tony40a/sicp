#lang sicp
;base^exp = base (mod exp) - если остаток от деления base^exp равен base, то exp-простое число
(define (square x) (* x x))
(define (modified-expmod base exp m) ;возвращает остаток от деления (mod) (пример modified-expmod 5 7 7)
  (cond ((= exp 0) 1)
        ((even? exp) 
         (remainder-or-trivial (modified-expmod base (/ exp 2) m);1-ая часть - fast-exp (быстрое возведение в степень base^exp) 
                    m)) 
        (else 
         (remainder (* base (modified-expmod base (- exp 1) m)) ;2-ая часть - fast-exp (быстрое возведение в степень base^exp) 
                    m))))

(define (trivial-check k m r)
    (if (and (not (= k 1)) 
           (not (= k (- m 1))) 
           (= r 1)) 
      0 
      r))

(define (remainder-or-trivial k m);проверка на тривиальное число миллера-рабина
  (trivial-check k m (remainder (square k) m)))

(define (fermat-test n) ;обычный тест Ферма
  (define (try-it a)
    (= (modified-expmod a n n) a));возврат true a^n=a(mod n)
  (try-it (+ 1 (random (- n 1)))));для случайного числа < n

(define (fast-prime? n times) ;итеративный прогон теста Ферма times раз
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)));Если true то вызываем следующую итерацию дла случайного а из fermat-test
        (else false)));если встретилось false для кого-то числа a, то n-не простое (не включая ложные простые чилсл Кармайкла)

(define (miller-rabin-test n) ;тест Миллера-Рабина
  (define (miller-rabin-iteration a t n) 
    (define (try-it a) 
      (= (modified-expmod a (- n 1) n) 1)) ;проверяем что a^(n-1)=1(mod n)
    (if (= a n) 
        (> t (/ n 2)) 
        (if (try-it a) 
            (miller-rabin-iteration (+ a 1) (+ t 1) n) 
            (miller-rabin-iteration (+ a 1) t n)))) 
  (miller-rabin-iteration 1 0 n));начинаем с а=1, t-сколько раз прошла проверка теста. В конце теста проверяем если t > n/2, то число простое

(miller-rabin-test 5)