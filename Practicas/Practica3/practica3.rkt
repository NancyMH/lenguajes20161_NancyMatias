#lang plai

(require "practica3-base.rkt")

;;;No entiendo las formulas cual es el valor de i, 0.,4,?
;;;Tampoco entiendo el problema, dado el minimo y el máximo hay que dar una zona?
;(define (zone resting maximum)

;Función auxiliar
;(define (range rest max)
 ; (cond
  ;  [(zero? max) rest]
   ; [(zero? rest) ((range*0.5)-1)]
    
  
;Tampoco entiendo el problema, pense que teniamos que buscar una zona que nos pasaban como parametro dentro de una lista de zonas, pero eso no
;tiene sentido, además por el ejmplo puesto en la practica creo que lo que estaba haciendo en el código de abajo esta mal.
(define (get-zone name lst-zones)
  (cond
    [(empty? lst-zones) empty]
    [(equal? name (car lst-zones)) car lst-zones]
    [else (get-zone name (cdr lst-zones))]))

;Hojas no vacías
(define (nlBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (cond
             [(not (or (EmptyBT? l) (EmptyBT? r))) (+ (nlBT l) (nlBT r))]
             [else 1])]))
(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (EmptyBT) 9 (EmptyBT))) 1)
(test (nlBT (BNode < (BNode < ( BNode < (EmptyBT) 4 (EmptyBT))6 (BNode < (EmptyBT) 5 (EmptyBT))) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nlBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 5 (EmptyBT)) 8 (EmptyBT)) 10 (EmptyBT)) 1 (EmptyBT)))1)
(test (nlBT (BNode < (EmptyBT) 18 (BNode < (EmptyBT) 20 (EmptyBT)))) 1)

;(printBT (bnn (bnn ebt 1 ebt) 2 (bnn ebt 3 (bnn ebt 4 ebt))))
;(printBT ( bnn (bnn (bnn ebt 4 ebt) 6 (bnn ebt 5 ebt)) 1 (bnn ebt 2 ebt)))
;(printBT ( bnn (bnn ebt 3 ebt) 1 (bnn ebt 2 ebt)))
;(printBT (bnn (bnn ebt 1 ebt) 2 (bnn ebt 3 (bnn ebt 4 ebt))))

;Nodos totales
(define (nnBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r) 
           (+ 1 (+ (nnBT l) (nnBT r)))]))
(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT (BNode < (BNode < ( BNode < (EmptyBT) 5 (EmptyBT))4 (BNode < (EmptyBT) 9 (EmptyBT))) 12 (BNode < (EmptyBT) 6 (EmptyBT)))) 5)
(test (nnBT (BNode < (BNode < (EmptyBT) 7 (EmptyBT)) 10 (EmptyBT))) 2)
(test (nnBT (BNode < (EmptyBT) 10 (EmptyBT))) 1)

;Nodos internos
(define (ninBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (cond
             [(and (EmptyBT? l) (EmptyBT? r)) 0]
             [else (+ 1 (+ (ninBT l) (ninBT r)))])]))

(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (EmptyBT) 1 (EmptyBT))) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT (BNode < (BNode < (EmptyBT) 7 (EmptyBT)) 10 (EmptyBT))) 1)
(test (ninBT (BNode < (BNode < ( BNode < (EmptyBT) 5 (EmptyBT))4 (BNode < (EmptyBT) 9 (EmptyBT))) 12 (BNode < (EmptyBT) 6 (EmptyBT)))) 2)

;mapBT
(define (mapBT fun arbol)
  (type-case BTree arbol
    [EmptyBT () (EmptyBT)]
    [BNode (c l e r)
           (BNode (fun e) (mapBT fun l) (mapBT fun r))]))
(test (mapBT add1 (EmptyBT))(EmptyBT))
;(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))

; Preorden del árbol, aún no funcionando
(define (preorderBT arbol-base)
  (type-case BTree arbol-base
    [EmptyBT () '()]
    [BNode (c l e r)
             ;[(not (or (EmptyBT? l) (EmptyBT? r))) (cons e ((preorderBT l) (preorderBT r)))]
             ;[else '()])]))
           (cond
             [(EmptyBT? l) (preorderBT r)]
             [(EmptyBT? r) (preorderBT l)]
             [else (cons e ((preorderBT l) (preorderBT r)))])]))
             ;(cons e (aux l r))]))
(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
