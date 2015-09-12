#lang plai

;SECCION 1:
;This is a function call any?. If variable is of any type of racket, it'll return true.
(define (any? var) #t) 

;Array
(define-type Array
  [MArray (length number?) (lst list?)])

;MList
(define-type MList
  [MEmpty]
  [MCons (element number?) (a MList?)])

;NTree
(define-type NTree
  [TLEmpty]
  [NodeN (element any?) (lst list?)])


;POSITION
(define-type Position
  [2D-Point (number1 real?) (number2 real?)])


;FIGURE
(define-type Figure 
  [Circle (pos Position?) (radio real?)]
  [Square (pos Position?) (longitud real?)]
  [Rectangle (pos Position?) (ancho real?) (largo real?)]
)  

;SECCION 2

;mapML
;Recibira una función y una lista, la lista sera del tipo que definimos en las funciones anteriores, sera de tipo MList.
;Si la lista que nos pasan es vacia, regresara vacio. En caso contrario definimos un MCons y le aplicamos la función a los
;parametros de MCons.

(define (mapML fun lst)
  (type-case MList lst
    [MEmpty () (MEmpty)]
    [MCons (num lst)
           (MCons (fun num) (mapML fun lst))]))

;Testing
(test (mapML add1 (MCons 7 (MCons 4 (MEmpty)))) (MCons 8 (MCons 5 (MEmpty))))
(test (mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 100 (MCons 9 (MEmpty))))
(test (mapML sub1 (MCons 10 (MCons 8 (MCons 6 (MCons 4 (MEmpty)))))) (MCons 9 (MCons 7 (MCons 5 (MCons 3 (MEmpty))))))
(test (mapML sub1 (MEmpty)) (MEmpty))
(test (mapML (lambda(y) (* y 2)) (MCons 30 (MCons 20 (MCons 10 (MEmpty))))) (MCons 60 (MCons 40 (MCons 20 (MEmpty)))))


;lengthML
(define (lengthML lst)
  (type-case MList lst
    [MEmpty () 0]
    [MCons (cabeza resto)
           (+ 1 (lengthML resto))]))
;testing
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)
(test (lengthML (MCons 1 (MEmpty))) 1)
(test (lengthML (MCons 3 (MCons 6 (MCons 8 (MEmpty))))) 3)
(test (lengthML (MCons 8 (MCons 7 (MCons 8 (MCons 4 (MEmpty)))))) 4)

;concatML
(define (concatML lst1 lst2)
  (type-case MList lst1
    [MEmpty () lst2]
    [MCons (cabeza resto)
            (MCons cabeza (concatML resto lst2))]))
;testing
(test (concatML (MCons 7 (MCons 10 (MEmpty))) (MEmpty)) (MCons 7 (MCons 10 (MEmpty)))) 
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty))))) 
(test (concatML (MEmpty) (MEmpty)) (MEmpty))
(test (concatML (MEmpty) (MCons 1 (MEmpty))) (MCons 1 (MEmpty)))
(test (concatML (MCons 11 (MCons 17 (MEmpty))) (MEmpty)) (MCons 11 (MCons 17 (MEmpty))))

;Define a function call it pi for that value y then can use it with the area of the circle
(define pi 3.1415926535897932)

;area
(define (area figura)
  (type-case Figure figura
    [Circle (pto rdio) (* pi (* rdio rdio))]
    [Square (pto tam) (* tam tam)]
    [Rectangle (pto a l) (* a l)]
 ))
(test (area (Circle (2D-Point 0 0) 6)) 113.0976)
(test (area (Square (2D-Point 0 1) 5)) 25)
(test (area (Square (2D-Point 0 0) 12)) 144)
(test (area (Rectangle (2D-Point 3 4) 5 10)) 50)
(test (area (Rectangle (2D-Point 3 4) 2 10)) 20)

;Marray2Mlist
(define (MArray2MList arr)
  (type-case Array arr
    [MArray (tam list) 
            (cond 
              [(zero? tam) (MEmpty)]
              [else(MCons (cdr list) MArray2MList(arr))] )]
     
 ))
(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))


