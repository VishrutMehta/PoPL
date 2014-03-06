#lang racket
(define (dollar->euro dollar)
    (* dollar 0.76))

(provide dollar->euro)
