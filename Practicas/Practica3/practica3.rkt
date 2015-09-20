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

;Metodo que obtiene el numero de nodos en el arbol

(define (nlBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r) 
           (+ 1 (+ (nlBT l) (nlBT r)))]))
(test (nlBT (EmptyBT)) 0)
(test(nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
