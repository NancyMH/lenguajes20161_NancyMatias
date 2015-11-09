#lang plai

;(print-only-errors true)

(define-type Binding
  [bind (name symbol?) (value RCFAEL?)])

(define-type RCFAEL
  [num (n number?)]
  [id (name symbol?)]
  [bool (value boolean?)]
  [binop (f procedure?)
         (l RCFAEL?)
         (r RCFAEL?)]
  [fun (params (listof symbol?))              
       (body RCFAEL?)])

(define-type RCFAELS
  [numS (n number?)]
  [idS (name symbol?)]
  [binopS (f procedure?)
          (l RCFAELS?)
          (r RCFAELS?)]
  [boolS (value boolean?)]
  [funS (params (listof symbol?))              
       (body RCFAELS?)]
  )


(define-type RCFAEL-Value
  [numV (n number?)]
  [boolV (value boolean?)]
  [closureV (param (listof symbol?))
            (body RCFAEL?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value RCFAEL-Value?) 
        (env Env?)])

 ;[Mlist (lst list?)                 
   ;      (car symbol?)
    ;     (cdr list?)]
  ;[with (bindings (listof bind?))        
  ;       (body RCFAEL?)]
  ;[rec (name-fun symbol?)           
   ;    (params RCFAEL?)
    ;   (body-fun RCFAEL?)]
  ;[ifR (exp RCFAEL?)         
       ;(res RCFAEL?)]
  ;[equalR? (exp1 RCFAEL?)
   ;       (exp2 RCFAEL?)]
  


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (desugar expr)
  (type-case RCFAEL expr
    [id (name) (id name)]
    [num (n) (num n)]
    [bool (value) (bool value)]
    [binop (exp l r) (binop exp
                            (desugar l)
                            (desugar r))]
    [fun (params body) (fun params (desugar body))]))

(test (desugar (id 'n)) (id 'n))
(test (desugar (num 8)) (num 8))
(test (desugar (bool #t)) (bool #t))


(define (cparse sexp)
  (desugar (parse sexp)))

(define (interp expr env)
  (type-case RCFAEL expr
    [num (n) (numV n)]
    [id (name) (lookup name env)]
    [binop (f l r) (num+ f (interp l env) (interp r env))]
    [bool (value) (bool value)]
    [fun (params body) 
         (closureV (lambda (arg-val)
                     (interp params
                             (aSub params arg-val env))))]))

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




    ;[bool (exp) (bool exp)]
    ;;[Mlist lst                                      ;;Marca un error que no entiendo
      ;;     [(empty? lst) empty]
        ;;   [cons (car lst)(cdr lst)]]
    ;[with (bindings body) 
     ;      (app (fun (map (lambda(bind) 
      ;                      (bind-name bind)) bindings)
       ;              (desugar body))
        ;   (map (lambda (bind) 
         ;         (desugar (bind-val bind))) bindings))]
    ;[with (bindings body) (matryoshka bindings body)]
    ;[rec (name-fun params body-fun) (name-fun (- params  1)  (desugar body-fun))]
    ;[fun (params body) (fun params (desugar body))]
    ;[ifR (exp res1 res2) (ifR (desugar exp)               ;;No creo que esto este bien, no se me ocurre como hacerlo
     ;                    (bool #t)                        ;;(desugar res1)
     ;                    (bool #f))]                      ;;(desugar res2)
    ;[equalR? (exp1 exp2) (equal?
     ;       [(equal? exp1 exp2) (desugar (bool #t))]
      ;      [(!equal? exp1 exp2) (desugar(bool #f))])]
  
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(boolean? sexp) (bool sexp)]
    [(list? sexp)
     (case (car sexp)
       [(+ - / * < > <= >= and or) (binop (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [else #t])]))
       ;[(with) (with (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       ;[(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       ;[(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       ;[else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse #f)) (bool #f)) 
;(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
;(test (rinterp (cparse '{> 3 4})) (bool #f))


;(define (matryoshka bindings body)
 ; (cond
  ;  [(empty? bindings) (desugar body)]
  ;  [else (app (fun (list (bind-name (car bindings)))
    ;                (matryoshka (cdr bindings) body))
   ;            (list (desugar (bind-val (car bindings)))))]))


