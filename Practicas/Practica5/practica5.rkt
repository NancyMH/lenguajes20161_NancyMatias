#lang plai

(define-type RCFAEL
  [id (name symbol?)]
  [num (n number?)]
  [bool (exp boolean?)]
  [Mlist (lst list?)                 
         (car symbol?)
         (cdr list?)]
  [with (bindings (listof bind?))        
         (body RCFAEL?)]
  [rec (name-fun symbol?)           
       (params RCFAEL?)
       (body-fun RCFAEL?)]
  [fun (params RCFAEL?)              
       (body RCFAEL?)]
  [ifR (cond RCFAEL?)         
       (res2 RCFAEL?)]
  [equalR? (exp1 RCFAEL?)
          (exp2 RCFAEL?)]
  [op (exp procedure?)
      (res RCFAEL?)]
  [binop (exp procedure?)
         (l RCFAEL?)
         (r RCFAEL?)])



(define-type Binding
  [bind (name symbol?) (value RCFAEL?)])

(define (operador op)
  (case op
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) (lambda (x y) (and x y))]
    [(or) (lambda (x y) (or x y))]))


(define (desugar expr)
  (type-case RCFAEL expr
    [id (name) (id name)]
    [num (n) (num n)]
    [bool (exp) (bool exp)]
    ;;[Mlist lst                                      ;;Marca un error que no entiendo
      ;;     [(empty? lst) empty]
        ;;   [cons (car lst)(cdr lst)]]
    [with (bindings body) 
           (app (fun (map (lambda(bind) 
                            (bind-name bind)) bindings)
                     (desugar body))
           (map (lambda (bind) 
                  (desugar (bind-val bind))) bindings))]
    [with* (bindings body) (matryoshka bindings body)]
    [rec (name-fun params body-fun) (name-fun (- params  1)  (desugar body-fun))]
    [fun (params body) (fun params (desugar body))]
    [ifR (cond then else) (ifR (desugar cond)               ;;No creo que esto este bien, no se me ocurre como hacerlo
                         (bool #t)
                         (bool #f))]
    [equalR? (exp1 exp2) (equal?
            [(equal? exp1 exp2) (desugar (bool #t))]
            [(!equal? exp1 exp2) (desugar(bool #f))])]
    [op (exp res) (op exp (desugar res))]
    [binop (exp l r) (binop exp
                            (desugar l)
                            (desugar r))]))



(define (matryoshka bindings body)
  (cond
    [(empty? bindings) (desugar body)]
    [else (app (fun (list (bind-name (car bindings)))
                    (matryoshka (cdr bindings) body))
               (list (desugar (bind-val (car bindings)))))]))
