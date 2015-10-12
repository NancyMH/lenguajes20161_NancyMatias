#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [withS (bindings body) (app (fun (map (lambda(x) (bind-name x)) bindings)
                                     (desugar body))
                                (map (lambda (x) (desugar (bind-val x))) bindings))]
    [with*S (bindings body) (matryoshka bindings body)]
    [idS (name) (id name)]
    [funS(params body) (fun params (desugar body))]
    [appS(fun lst) (app (desugar fun) (desugar lst))]
    [binopS(fun l r) (binop fun(desugar l) (desugar r))]))
  

(define (matryoshka bindings body)
              (cond
                  [(empty? bindings) (desugar body)]
                  [else (app (fun bindings(matryoshka(cdr bindings)
			(desugar bindings))))]))



                                ;;;;;Tests;;;;;
;NOTA: Si no detecta los tests al momento de ejecutarlo, hay que realizar las pruebas en la linea de comandos.
(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (desugar (parse '(fun (x) x))) (fun '(x) (id 'x)))
(test (desugar (numS 8)) (num 8))
(test (parse '(fun (x) x)) (funS '(x) (idS 'x)))
(test (desugar (idS 'n)) (id 'n))


(define (cparse sexp)
  (desugar (parse sexp)))

;En esta función se logró implementar sólo los casos numV, id (con ayuda de la función lookup descrita más adelante)
;fun y binop (con ayuda de la función num+(la cual se encarga de llevar a cabo la operación pertinente con el par
;de números que reciba como parámetro); se basa en los ejercicios vistos en clase así como la ayuda del libro Shriram.
(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [id (name) (lookup name env)]
    [fun (params body) 
         (closureV (lambda (arg-val)
                     (interp params
                             (aSub params arg-val env))))]
    [app (fun args)(#t)]
    [binop (f l r) (num+ f (interp l env) (interp r env))]))

;Llama al intérprete con la expresión y el ambiente.
(define (rinterp expr)
  (interp expr (mtSub)))

;Lleva a cabo la opreción correspondiente con el par de números que recibe como parámetros;
(define (num+ f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

;Revisa si el id está dentro del ambiente. 
(define (lookup name env)
 (type-case Env env
   [mtSub () (error 'lookup "symbol is not in the env")]
   [aSub (bound-name bound-value rest-env)
         (if(symbol=? bound-name name)
            bound-value
            (lookup name rest-env))]))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
;(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
;(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
;(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
;(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
;(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
;(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
;(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
;(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
;(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
;(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
;(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
;(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
;(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
;(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
;(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
;(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))
