#lang racket
(define (convert3 first second third)
    (+ (+ first (* second 10)) (* third 100))) 

(provide convert3)
