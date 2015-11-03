#lang plai

(define (desugar expr)
  (type-case RCFAEL expr
    [id (name) (id name)]
    [num (n) (num n)]
    [bool (exp) (bool exp)]
    [Mlist lst
           [(empty? lst) empty]
           [cons (car lst)(cdr lst)]]
    [with (bindings body) 
           (app (fun (map (lambda(bind) 
                            (bind-name bind)) bindings)
                     (desugar body))
           (map (lambda (bind) 
                  (desugar (bind-val bind))) bindings))]
    [rec (name-fun params body-fun) (name-fun (- params  1)  (desugar body-fun))]
    [fun(params body) (fun params (desugar body))]
    [if (cond) (desugar #t)
               (desugar #f)]
    [equal? (exp1 exp2) 
            [(equal? exp1 exp2) (bool #t)]
            [(!equal? exp1 exp2) (bool #f)]]))
    
