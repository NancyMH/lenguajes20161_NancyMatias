#lang plai

(require "practica3-base.rkt")

;;;Function zones
;In this function We are going to obtain the minimum and maximum for each zone by applying the respective formulae.
;The variable i in each formula will take a value between 0 and 4, where:
;For warm-up i=0 
;For fat-burning i= 1 
;For aerobics i= 2 
;For anaerobic i= 3 
;For maximum i=4

(define (zones rest max )
    [list (resting rest (+ rest(- (* (range rest max) 0.5) 1)))
          (warm-up (+ rest (* (range rest max) (+ 0.5 (* 0.1 0)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 1))) 1)))
          (fat-burning (+ rest (* (range rest max) (+ 0.5 (* 0.1 1)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 2))) 1)))
          (aerobic (+ rest (* (range rest max) (+ 0.5 (* 0.1 2)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 3))) 1)))
          (anaerobic (+ rest (* (range rest max) (+ 0.5 (* 0.1 3)))) (+ rest (- (* (range rest max) (+ 0.5 (* 0.1 4))) 1)))
          (maximum (+ rest (* (range rest max) (+ 0.5 (* 0.1 4)))) (+ rest (* (range rest max) (+ 0.5 (* 0.1 5)))))])


;Auxiliary Function to get the range between the minimum and maximum, for which we will subtract the minimum to the maximum
(define (range rest max)
    (- max rest))

;Define it for example
(define my-zones (zones 50 180))

;Test
(test (zones 50 180)
 (list
 (resting 50 114.0)
 (warm-up 115.0 127.0)
 (fat-burning 128.0 140.0)
 (aerobic 141.0 153.0)
 (anaerobic 154.0 166.0)
 (maximum 167.0 180.0)))

(test (zones 20 80)
 (list
 (resting 20 49.0)
 (warm-up 50.0 55.0)
 (fat-burning 56.0 61.0)
 (aerobic 62.0 67.0)
 (anaerobic 68.0 73.0)
 (maximum 74.0 80.0)))

(test (zones 5 5)
 (list (resting 5 4) (warm-up 5 4) (fat-burning 5 4) (aerobic 5 4) (anaerobic 5 4) (maximum 5 5)))

(test (zones 100 500)
 (list
 (resting 100 299.0)
 (warm-up 300.0 339.0)
 (fat-burning 340.0 379.0)
 (aerobic 380.0 419.0)
 (anaerobic 420.0 459.0)
 (maximum 460.0 500.0)))

(test (zones 0 75)
 (list (resting 0 36.5)
 (warm-up 37.5 44.0)
 (fat-burning 45.0 51.5)
 (aerobic 52.5 59.0)
 (anaerobic 60.0 66.5)
 (maximum 67.5 75.0)))


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
