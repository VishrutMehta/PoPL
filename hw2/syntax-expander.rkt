#lang racket

(require eopl/eopl)
(provide run
         syntax-expander)
(define *op-symbols*
  '(+ - * /
    < <= > >= eq? 0?))

;;; op-symbol? : symbol? -> bool
(define op-symbol?
  (lambda (x)
    (if (memq x *op-symbols*)
      #t
      #f)))

(define if-symbol?
  (lambda (x)
    (if (memq x '(my-if))
      #t
      #f)))

(define my-assume?
  (lambda (x)
    (if (eqv? x 'assume)
      #t
      #f)))


(define-datatype bind bind?
  [make-bind (id symbol?)(ast ast?)])

(define-datatype ast ast?
  [number (datum number?)]
  [boolean (datum boolean?)]
  [identifier (sym symbol?)]
  [prim-app (op op-symbol?) (rands (list-of ast?))]
  [if-ast (dat1 ast?) (dat2 ast?) (dat3 ast?)]
  [assume (keyword my-assume?) (rands (list-of bind?)) (expr ast?)])
  


(require rackunit)

;;;parser.ss
(define parse-error
  (lambda (d)
    (error 'parse-error "invalid syntax ~a" d)))



(define syntax-expander
  (lambda (d)
    (cond
      [(number? d) (number d)]
      [(boolean? d) (boolean d)]
      [(symbol? d) (identifier d)]
      [(and
         (list? d)
         (not (null? d))
         (memq (first d) *op-symbols*))
       (prim-app (first d)
         (map syntax-expander (rest d)))]
      [(and
         (list? d)
         (not (null? d))
         (eqv? (first d) 'if))
       (if-ast (syntax-expander (second d)) (syntax-expander (third d)) (syntax-expander (fourth d)))]
      [(and
        (list? d)
         (not (null? d))
         (eqv? (first d) 'cond))
       (if (eq? (first (second d)) 'else) (second (second d)) (if-ast (syntax-expander (first (second d))) (syntax-expander (second (second d))) (if (eq? (first (first (rest (rest d)))) 'else) (syntax-expander (second (first (rest (rest d))))) (syntax-expander (append '(cond) (rest (rest d)))))))]
      [(and
       (list? d)
       (not (null? d))
       (eqv? (first d) 'assume))
      (assume (first d) (map syntax-expander (cadr d)) (syntax-expander (caddr d)))]
     [(and  
       (eq? (length d) 2) 
       (symbol? (first d))) 
      (make-bind (first d) (syntax-expander (cadr d)))]
      )))

;;; semantic domains
(define expressible-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))

;;; op.ss
;;; nonzero? : any/c -> boolean?
(define nonzero?
  (lambda (n)
    (and (number? n)
      (not (zero? n)))))

;;; Operators
;;; =========
(define-struct op (name prim sig))
(define +op   (make-op '+  +   (list number? number? number?)))
(define -op    (make-op '-     -     (list number? number? number?)))
(define *op    (make-op '*     *     (list number? number? number?)))
(define /op    (make-op '/     /     (list number? number? nonzero?)))
(define <op    (make-op '<     <     (list boolean? number? number?)))
(define <=op   (make-op '<=    <=    (list boolean? number? number?)))
(define >op    (make-op '>     >     (list boolean? number? number?)))
(define >=op   (make-op '>=    >=    (list boolean? number? number?)))
(define eq?op  (make-op 'eq?   eq?   (list boolean? expressible-value? expressible-value?)))
(define 0?op   (make-op '0?    zero? (list boolean? number?)))

(define *ops*
  (list +op -op *op /op <op <=op eq?op 0?op))

(define op-find
  (lambda (opsym)
    (findf (lambda (op)
             (eq? opsym (op-name op)))
           *ops*)))

;;;eval-ast
(define match-sig?
  (lambda (sig? val)
    (sig? val)))


(define apply-prim-op
  (lambda (opsym args)
    (let* ([op (op-find opsym)]
           [sig (op-sig op)]
           [args-sig (rest sig)])
      (cond
       [(and
         (= (length args-sig) (length args))
         (and map match-sig? args-sig args))
        (apply (op-prim op)  args)]
       [#t (error 'apply-prim-op "incorrect number or type of arguments to ~a" opsym)]))))



(define (bind-id b)
  (cases bind b
     [make-bind (id ast) id]))

(define (bind-ast b)
  (cases bind b
     [make-bind (id ast) ast]))


(define eval-ast
  (lambda (a e)
    (cases ast a
      [number (datum) datum]
      [boolean (datum) datum]
      [identifier (id) (lookup e id) ]
      [prim-app (op rands)
                (let ([args (map eval-ast rands (list e e))])
          (apply-prim-op op args))]
      [if-ast (arg1 arg2 arg3)
             (if (eval-ast arg1 e) (eval-ast arg2 e) (eval-ast arg3 e))]
      [assume (keyword binds body)
              (let ( [ids (map bind-id binds)] [vals (map (lambda(a) (eval-ast a e)) (map bind-ast binds))])
                (let ( [new-e (extended-env ids vals e)])
                  (eval-ast body new-e)))]
      )))

;;;Environment
(define denotable-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))

(define-datatype env env?
  [empty-env ]
  [extended-env (ids (list-of symbol?)) (dvals (list-of denotable-value?)) (outer-env env?)])


(define (list-index list number)
 (if (memq number list) (- (length list) (length (memq number list))) -1))



(define (lookup e x)
  (cases env e
    [empty-env() (error "unbound id va" x) ]
    [extended-env (ids dvals outer-env) 
         (let ( [j (list-index ids x)])
           (if (eq? j -1) (lookup outer-env x) (list-ref dvals j)))
                 ]
    ))

(define env1 (extended-env '(try) '(1) (empty-env)))
(define run
  (lambda (e)
    (eval-ast (syntax-expander e) env1)))
(provide boolean
         number 
         if-ast
         syntax-expander
         prim-app
         run)
