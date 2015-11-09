#lang plai

;(print-only-errors true)

;Declaración del tipo RCFAEL con sus respectivas variantes 
;Para cada caso se agregan los parámetros que se necesitarán en cada una de ellas.
(define-type RCFAEL
  [num (n number?)]
  [id (name symbol?)]
  [bool (value boolean?)]
  [binop (f procedure?)
         (l RCFAEL?)
         (r RCFAEL?)]
  [fun (params (listof symbol?))
       (body RCFAEL?)]
  [equalR? (exp1 RCFAEL?)
        (exp2 RCFAEL?)]
  [op (f procedure?)
      (value RCFAEL?)]
  [ifR (arg RCFAEL?) 
       (res1 RCFAEL?)
       (res2 RCFAEL?)]
  [rec (name-fun symbol?)           
       (params RCFAEL?)
       (body-fun RCFAEL?)])
  ;[with (bindings (listof bind?))        
   ;     (body RCFAEL?)]

;Declaración del tipo RCFAELS con sus respectivas variantes.
;Para cada caso se agregan los parámetros que se necesitarán en cada una de ellas.
(define-type RCFAELS
  [numS (n number?)]
  [idS (name symbol?)]
  [binopS (f procedure?)
          (l RCFAELS?)
          (r RCFAELS?)]
  [boolS (value boolean?)]
  [funS (params (listof symbol?))              
       (body RCFAELS?)]
  [equalRS? (exp1 RCFAELS?)
        (exp2 RCFAELS?)]
  [opS (f procedure?)
       (value RCFAELS?)]
  [ifRS (arg RCFAELS?) 
       (res1 RCFAELS?)
       (res2 RCFAELS?)]
  [recS (name-fun symbol?)           
        (params RCFAELS?)
        (body-fun RCFAELS?)])
;[withS (bindings (listof bind?))
 ;        (body RCFAELS?)]
  ;[with*S (bindings (listof bind?))



;Declaración de RCFAEL-Value, en donde simplemente agregamos el término
;de boolV para el rinterp.
(define-type RCFAEL-Value
  [numV (n number?)]
  [boolV (value boolean?)]
  [closureV (param (listof symbol?))
            (body RCFAEL?)
            (env Env?)])


;Se define el ambiente que se utilizará.
(define-type Env
  [mtSub ]
  [aSub (name symbol?)
        (value RCFAEL-Value?)
        (env Env?)]
  [aRecSub (name symbol?)
            (value boxed-RCFAEL-Value?)
            (env Env?)])



; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))


;Para las operaciones binarias, se recibe una operación dada una expresión y regresa
;el ya definido por Racket. 
(define (elige op)
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


;Recibido un elemento, regresa la acción que ya está definida en 
;racket para llevar a cabo la operación correspondiente para operaciones unarias.
(define (eligeUnario op)
  (case op
    [(inc) add1]
    [(dec) sub1]
    [(zero?) zero?]
    [(num?) number?]
    [(neg) not]
    [(bool?) boolean?]
    [(first) first]
    [(rest) rest]
    [(empty?) empty?]
    [(list?) list?]))

  
;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))


;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))


;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RCFAEL
(define (desugar expr)
  (type-case RCFAELS expr
    [idS (name) (id name)]
    [numS (n) (num n)]
    [boolS (value) (bool value)]
    [binopS (exp l r) (binop exp
                            (desugar l)
                            (desugar r))]
    [funS (params body) (fun params (desugar body))]
    [equalRS? (exp1 exp2)(equalR? (desugar exp1) (desugar exp2))]
    [opS (f value) (op f (desugar value))]
    [ifRS (arg res1 res2) (ifR (desugar arg) 
                               (desugar res1)
                               (desugar res2))]
    [recS (name-fun params body-fun) (rec (desugar name-fun)
                                          (desugar params)
                                          (desugar body-fun))]
    ;[with (bindings body) 
     ;      (app (fun (map (lambda(bind) 
      ;                      (bind-name bind)) bindings)
       ;              (desugar body))
        ;   (map (lambda (bind) 
         ;         (desugar (bind-val bind))) bindings))]
    ;[with* (bindings body) (matryoshka bindings body)]
    ))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Binding
  [bind (name symbol?) (value RCFAELS?)])

;Realizamos el parse
(define (cparse sexp)
  (desugar (parse sexp)))

;Para cada variante de RCFAEL donde ya ha pasado previamente por el parse,
;se encarga del interprete, donde llevará a cabo la operación que le corresponda
;y de acuerdo a los parámetros que necesita.
(define (interp expr env)
  (type-case RCFAEL expr
    [num (n) (numV n)]
    [id (name) (lookup name env)]
    [binop (f l r) (num+ f (interp l env) (interp r env))]
    [bool (value) (boolV value)]
    [fun (params body) 
         (closureV (lambda (arg-val)
                     (interp params
                             (aSub params arg-val env))))]
    [equalR? (exp1 exp2)(auxeq (interp exp1 env) (interp exp2 env))]
    [op (f value) (auxUnario f (interp value env))]
    [ifR (arg res1 res2) (bool (interp arg env)
                               (interp res1 env)
                               (interp res2 env))]
    [rec (name-fun params body-fun) (rec (interp name-fun env)
                                         (interp params env)
                                         (interp body-fun env))]
    ))

;;Función auxiliar de op que lleva a cabo las operaciones unarias, recibe una opeación y el elemento a evaluar.
;Regresa un #t o #f según sea el caso para este tipo de operación.
(define (auxUnario f v1)
  (boolV (zero? (numV v1) )))


;Llama al intérprete con la expresión y el ambiente.
(define (rinterp expr)
  (interp expr (mtSub)))

;Función auxiliar de binop que lleva a cabo la operación correspondiente con el par de elementos que recibe como parámetros,
;y regresa el resultado de haberle aplicado dicha operación

(define (num+ f n1 n2)
  (cond
    [(and (numV? n1) (numV? n2))(numV (f (numV-n n1) (numV-n n2)))] 
    [(and (boolV? n1) (boolV? n2) (eqv? 'and f)) (boolV (and (boolV n1) (boolV n2)))]
    [else "La aplicación de binop no es adecuada"]))

;Función auxiliar de equal? que compara dos valores, ya sea números o
;booleanos, en otro caso se regeresa al usuario el mal uso de esta operación.
(define (auxeq v1 v2)
  (cond
    [(and (numV? v1) (numV? v2) ) (boolV (equal? v1 v2))]
    [(and (boolV? v1) (boolV? v2)) (boolV (equal? v1 v2))]
    [else "La aplicación de equal? no es adecuada"]))


;Revisa si el id está dentro del ambiente. 
(define (lookup name env)
  (type-case Env env
    [ mtSub () (error 'lookup "symbol is not in the env")]
    [ aSub (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
              (if (symbol=? bound-name name)
                  (unbox boxed-bound-value)
                  (lookup name rest-env))]))


;Definimos la caja con el valor
(define (boxed-RCFAEL-Value? v)
  (and (box? v)
       (RCFAEL-Value? (unbox v))))



;;cyclically-bind-and-interp : symbol RCFAE env → env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box ( numV 1729 ))]
          [define new-env ( aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

  
(define (parse sexp)
  (cond
    [(eqv? 'true sexp) (boolS #t)]
    [(eqv? 'false sexp) (boolS #f)]
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(equal?) (equalRS? (parse (cadr sexp)) (parse (caddr sexp)))]
       [(+ - / * < > <= >= and or) (binopS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(inc dec zero? number? neg bool? first rest empty? list?) (opS (eligeUnario (car sexp)) (parse (cadr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(ifR) (ifRS (parse (car sexp)) 
                   (parse (cadr sexp))
                   (parse (caddr sexp)))]
       [(rec) (recS (caadr sexp)
                    (parse (cadadr sexp))
                    (parse (caddr sexp)))]
       [else #t])]))
       ;[(with) (with (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       ;[(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       ;[else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))


;;;;;;;;;;;;;Pruebas
(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse 'true)) (boolV #t)) 
(test (rinterp (cparse 'false)) (boolV #f)) 
(test (rinterp (cparse '{equal? 5 1})) (boolV #f))
(test (rinterp (cparse '{equal? 10 10})) (boolV #t))
(test (rinterp (if (>= 17 7) (num 17) (num 0))) (numV 17))
(test (rinterp (if (=(* 3 7) 21) (num 1) (num 0))) (numV 1))
(test (rinterp (if (= 4 8) (bool #t) (bool #f))) (boolV #f))
(test (rinterp (cparse (if '{<= 15 5} 1 0))) (numV 1))
(test (rinterp (cparse (if '{={/ 20 5} 4} 4 0))) (numV 4))



