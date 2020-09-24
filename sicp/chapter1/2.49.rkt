#lang sicp
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (get-origin frame)
  (car frame))

(define (get-edge1 frame)
  (cadr frame))

(define (get-edge2 frame)
  (caddr frame))

(define (make-segment v-start v-end) ;формирование отрезка для рисования 
  (cons v-start v-end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))


(define frame-test (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

;четыре отрезка стороны рамки:
;сторона edge1
(define frame-outline1 (lambda (frame) (make-segment (get-origin frame) (add-vect ((get-origin frame) (get-edge1 frame))))))
;сторона edge2
(define frame-outline2 (lambda (frame) (make-segment (get-origin frame) (add-vect ((get-origin frame) (get-edge2 frame))))))
;сторона параллельная edge1
(define frame-outline3 (lambda (frame) (make-segment (get-edge2 frame) (add-vect (get-edge1 frame) (get-edge2 frame)))))
;сторона параллельная edge2
(define frame-outline4 (lambda (frame) (make-segment (get-edge1 frame) (add-vect (get-edge1 frame) (get-edge2 frame)))))

;--------------------------------------------------------------------------------------------------------------------------

;отрезки соединяющие углы рамки X:
(define frame-x1 (lambda (frame) (make-segment (get-origin frame) (add-vect (get-edge1 frame) (get-edge2 frame)))))
(define frame-x2 (lambda (frame) (make-segment (get-edge1 frame) (get-edge2 frame))))

;--------------------------------------------------------------------------------------------------------------------------

;вектор-середина для сегмента seg 
(define (median-vector frame seg)
  (make-vect (/ (+ (xcor-vect (start-segment (seg frame))) (xcor-vect (end-segment (seg frame)))) 2)
             (/ (+ (ycor-vect (start-segment (seg frame))) (ycor-vect (end-segment (seg frame)))) 2))) 

;отрезок, соединяющий середины edge1 и параллельную edge1
(define frame-center-edge1 (lambda (frame) (make-segment (median-vector frame frame-outline1) (median-vector frame frame-outline3))))

;отрезок, соединяющий середины edge2 и параллельную edge2
(define frame-center-edge2 (lambda (frame) (make-segment (median-vector frame frame-outline2) (median-vector frame frame-outline4))))

;--------------------------------------------------------------------------------------------------------------------------

;все векторы выходят из (0;0)
;рамка:
(define outline-painter
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 0))
                           (make-segment (make-vect 1 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 1)
                                         (make-vect 0 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 0 0)))))

;X:
(define X-painter
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 0)
                                         (make-vect 0 1)))))

;diamond
(define diamond-painter
  (segments->painter (list (make-segment (make-vect 0.5 0)
                                         (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5)
                                         (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1)
                                         (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5)
                                         (make-vect 0.5 0)))))

;вспомогательные процедуры для ломаной линии и замкнутого контура
;формирует список отрезков (конец отрезка -> начало следующего):
(define (polyline vectors)
  (if (null? (cdr vectors))
      null
      (cons (make-segment (car vectors) (cadr vectors))
            (polyline (cdr vectors)))))

;добавляет первый вектор в конец для формирования замкнутого контура
(define (contour vectors)
  (polyline (append vectors
                    (list (car vectors)))))

;рисовалка wave
(define wave-painter 
  (segments->painter 
    (append (polyline (make-vect 0 0.15) 
                      (make-vect 0.15 0.4) 
                      (make-vect 0.3 0.35) 
                      (make-vect 0.4 0.35) 
                      (make-vect 0.35 0.15) 
                      (make-vect 0.4 0)) 
            (polyline (make-vect 0.6 0) 
                      (make-vect 0.65 0.15) 
                      (make-vect 0.6 0.35) 
                      (make-vect 0.75 0.35) 
                      (make-vect 1 0.65)) 
            (polyline (make-vect 1 0.85) 
                      (make-vect 0.6 0.55) 
                      (make-vect 0.75 1)) 
            (polyline (make-vect 0.6 1) 
                      (make-vect 0.5 0.7) 
                      (make-vect 0.4 1)) 
            (polyline (make-vect 0.25 1) 
                      (make-vect 0.35 0.5) 
                      (make-vect 0.3 0.4) 
                      (make-vect 0.15 0.6) 
                      (make-vect 0 0.35)))))



                           
                           












