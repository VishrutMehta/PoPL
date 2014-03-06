#lang racket
(define (area-cylinder radius height)
    (+ (* (* 2 pi) (* radius radius)) (* (* 2 pi) (* radius height))))

(provide area-cylinder)
