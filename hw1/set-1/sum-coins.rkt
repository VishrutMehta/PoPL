#lang racket
(define (sum-coins pen nic dim qut)
    (+ (+ (* pen 0.01) (* nic 0.05)) (+ (* dim 0.1) (* qut 0.25))))

(provide sum-coins)    
