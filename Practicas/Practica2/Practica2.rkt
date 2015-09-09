#lang plai
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
;POSITION
(define-type Position
  [2D-Point (number1 real?) (number2 real?)])
(2D-Point 0 0)
(2D-Point 1 (sqrt 2))
;FIGURE
(define-type Figure 
  [Circle (2D-Point (a real?) (b real?)) (radio real?)]
)
(Circle (2D-Point 2 2) 2)




