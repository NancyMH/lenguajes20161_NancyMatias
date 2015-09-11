#lang plai

;SECCION 1:

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




