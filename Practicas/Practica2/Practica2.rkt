#lang plai

;SECCION 1:

; Tipo any?
(define (any? var) #t) 

;Array
(define-type Array
  [MArray (length number?) (lst list?)])
(MArray 4 '(1 2 3)) 
(MArray 6 '(a a a d 4 d))


;MList
(define-type MList
  [MEmpty]
  [MCons (element number?) (a MList?)])
(MEmpty)
(MCons 1 (MCons 2 (MCons 3 (MEmpty))))


;NTree
(define-type NTree
  [TLEmpty]
  [NodeN (element any?) (lst list?)])
(NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty)))
(NodeN 1 (list (NodeN 2 (list (TLEmpty)))
               (NodeN 3 (list (TLEmpty)))
               (NodeN 4 (list (TLEmpty) (TLEmpty) (TLEmpty)))))


;POSITION
(define-type Position
  [2D-Point (number1 real?) (number2 real?)])
(2D-Point 0 0)
(2D-Point 1 (sqrt 2))


;FIGURE
(define-type Figure 
  [Circle (pos Position?) (radio real?)]
  [Square (pos Position?) (longitud real?)]
  [Rectangle (pos Position?) (ancho real?) (largo real?)]
)  
(Circle (2D-Point 2 2) 2)
(Square (2D-Point 0 3) 3)
(Rectangle (2D-Point 0 2) 2 3)


;SECCION 2

(define (mapML fun lst)
  (type-case MList lst
    [MEmpty () (MEmpty)]
    [MCons (num lst)
           (MCons (fun num) (mapML fun lst))]))

(mapML add1 (MCons 7 (MCons 4 (MEmpty))))
(mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty))))

;lengthML
(define (lengthML lst)
  (type-case MList lst
    [MEmpty () 0]
    [MCons (cabeza resto)
           (+ 1 (lengthML resto))]))
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)

;concatML
(define (concatML lst1 lst2)
  (type-case MList lst1
    [MEmpty () lst2]
    [MCons (cabeza resto)
            (MCons cabeza (concatML resto lst2))]))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty))))) 

(define pi 3.1415926535897932)

;area
(define (area figura)
  (type-case Figure figura
    [Circle (pto rdio) (* pi (* rdio rdio))]
    [Square (pto tam) (* tam tam)]
    [Rectangle (pto a l) (* a l)]
 ))
(test (area (Circle (2D-Point 5 5) 4)) 50.2656)
(test (area (Square (2D-Point 0 0) 20)) 400)
(test (area (Rectangle (2D-Point 3 4) 5 10)) 50)

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


