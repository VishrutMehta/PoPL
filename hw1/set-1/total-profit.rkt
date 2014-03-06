#lang racket
(define (total-profit people)
     (- (* 5 people) (+ 20 (* 0.5 people))))

(provide total-profit)
