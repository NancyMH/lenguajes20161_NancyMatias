#lang plai

;SECCION 1:
;This is a function call any?. If variable is of any type of racket, it'll return true.
(define (any? var) #t) 

;Array
;Here we are defining a data type of array, which we call MArray, which will have a constructor with a parameter that specify 
;the size that will take the settlement and a list.

(define-type Array
  [MArray (length number?) (lst list?)])

;MList
;Here we are defining a MList data type, this type of data is going to contain the empty list and its constructor will be declared 
;through the MCons function.

(define-type MList
  [MEmpty]
  [MCons (element number?) (a MList?)])

;NTree
;Here we are defining a data type NTree, this type of data contains to the null leaf. It has a constructor of trees n-Aryans
;type with NodeN.

(define-type NTree
  [TLEmpty]
  [NodeN (element any?) (lst list?)])


;POSITION
;Here we are defining a data type listing which requires a type constructor 2D-Point that is as parameter two real numbers that
;indicate a position in the Cartesian plane.

(define-type Position
  [2D-Point (number1 real?) (number2 real?)])


;FIGURE
;Here we are defining a data type figure, which requires three constructors to know what type of figure is. Its builders are: 
;Circle - The constructor takes the center given by the position and a radio.
;Square - A constructor that takes a position of the upper-left corner of the square and a length.
;Rectangle - A constructor that takes a position of the upper-left corner of the rectangle and the rectangle's length

(define-type Figure 
  [Circle (pos Position?) (radio real?)]
  [Square (pos Position?) (longitud real?)]
  [Rectangle (pos Position?) (ancho real?) (largo real?)])  

;SECCION 2

;mapML
;You will receive a function and a list, the list will be of the type that we defined in the previous functions, will be of type MList.
;If the list that we spend is empty, you will be returned empty. Otherwise we define a MCons and i am applying the function to the
;MCons parameters.

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


