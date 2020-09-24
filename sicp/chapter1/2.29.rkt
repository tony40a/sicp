#lang sicp
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch) ;вес ветви мобиля как веса ее структуры (гири либо подмобиля)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (total-weight struct)
        struct)))

(define (total-weight mobile) ;общий вес = вес правой + вес левой ветвей
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile) ;в этой процедуре вынуждены проходить все поддерево узла, чтобы определить его общую массу
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (balanced? (branch-structure branch))
        true))
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (torque left) (torque right)))))


;решение thror с накоплением весов и состояний
(define (make-residue state weight)
  (list state weight))

(define (residue-state residue)
  (car residue))

(define (residue-weight residue)
  (cadr residue))

(define (combine l r mobile)
  (make-residue (and (residue-state l)
                     (residue-state r)
                     (= (* (residue-weight l) (branch-length (left-branch mobile)))
                        (* (residue-weight r) (branch-length (right-branch mobile)))))
                (+ (residue-weight l) (residue-weight r))))

(define (reduce mobile)
  (cond ((null? mobile) (make-residue true 0))
        ((not (pair? mobile)) (make-residue true mobile))
        (else (let ((l (reduce (branch-structure (left-branch mobile))))
                    (r (reduce (branch-structure (right-branch mobile)))))
                (combine l r mobile)))))

(define (total-weight mobile)
  (residue-weight (reduce mobile)))

(define (balanced? mobile)
  (residue-state (reduce mobile)))