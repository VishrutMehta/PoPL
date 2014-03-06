#lang racket
(define (attendees ticp)
    (+ 120 (* 150.0 (- 5 ticp))))

(define (cost ticp)
    (* 1.5 (attendees ticp)))

(define (revenue ticp)
    (* ticp (attendees ticp))) 

(define (profit ticp)
    (- (revenue ticp) (cost ticp))) 

(provide attendees)
(provide cost)    
(provide revenue)  
(provide profit)  
