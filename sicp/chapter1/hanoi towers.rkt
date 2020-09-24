#lang sicp
;перенести n-1 диск со шпиля a на шпиль c, используя b как вспомогательный;
;перенести единственный оставшийся на шпиле a самый большой диск на шпиль b;
;перенести n-1 диск со шпиля c на шпиль b, используя a как вспомогательный.
(define (hanoi n src dest inter) 
  (if (= n 0) 
      nil 
      (append (hanoi (- n 1) src inter dest) 
              (list (list src dest)) 
              (hanoi (- n 1) inter dest src))))


(hanoi 6 1 2 3)