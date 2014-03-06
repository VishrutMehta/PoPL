#lang racket
(define (Fahrenheit->Celsius Fahrenheit)
   (* (- Fahrenheit 32) (/ 5 9.0) ))

(provide Fahrenheit->Celsius)
