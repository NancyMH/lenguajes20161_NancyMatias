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
