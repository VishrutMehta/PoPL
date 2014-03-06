#lang racket
(define (area-pipe r L t)
    (* (* 2 pi) (* (+ t L) (+ t (* 2 r)))))

(provide area-pipe)
