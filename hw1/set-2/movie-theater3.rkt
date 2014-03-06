#lang racket
(define (people) 120)
(define (incur_cost) 180)
(define (cost_per_person) 0.04)
(define (initial_cost) 5)
(define (inc_att) 150.0)      

(define (attendees ticp)
     (+ (people) (* (inc_att) (- (initial_cost) ticp))))

(define (cost ticp)
    (+ (incur_cost) (* (cost_per_person) (attendees ticp)))) 

(define (revenue ticp)
    (* ticp (attendees ticp))) 

(define (profit ticp)
    (- (revenue ticp) (cost ticp))) 

(provide attendees)
(provide cost)    
(provide revenue)  
(provide profit)  
