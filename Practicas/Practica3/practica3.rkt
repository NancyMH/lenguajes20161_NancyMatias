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



;;;Function get-zone
;From a list of zones we are going to get data from a specific zone in which we would like, (which we do through this function,
;which you will receive as parameters the list of zones and the name of a zone) for which, first discovered that zone we are
;looking for, comparing each name of the zones with the symbol that we received as a parameter. Once we know the name of the zone
;where we want, we proceed to look for it in the list of zones, for which we searched first in the head, if it's there, we 
;returned the head, but, we continue the search in the rest of the list.

(define (get-zone symbol lst-zones)
  (cond
    [(empty? lst-zones) empty]
    [(and (equal? 'resting symbol) (resting? (car lst-zones))) (car lst-zones)]
    [(and (equal? 'warm-up symbol) (warm-up? (car lst-zones))) (car lst-zones)]
    [(and (equal? 'fat-burning symbol) (fat-burning? (car lst-zones))) (car lst-zones)]
    [(and (equal? 'aerobic symbol) (aerobic? (car lst-zones))) (car lst-zones)]
    [(and (equal? 'anaerobic symbol) (anaerobic? (car lst-zones))) (car lst-zones)]
    [(and (equal? 'maximum symbol) (maximum? (car lst-zones))) (car lst-zones)]
    [else (get-zone symbol (cdr lst-zones))]))

;Test
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180.0))
(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))



;;bpm->zone function
;It doesn't work
(define (bpm->zone lst-frec lst-zones frec)
  (cond
    [(empty? lst-frec) empty]
    [(empty? lst-zones) empty]
    [else (status lst-zones frec)]))

     
;Auxiliary Function that return if a zone is is within the range of frequency
;I am comparing the frequency that we are going to the range between minimum and maximum, once we see that if it comes, 
;look for the item that matches that frequency in the zone list
;It doesn't work
(define (status lst-zones frec)
  [type-case HRZ lst-zones
    (resting(low high) (if (and (>= frec low) (<= frec high)) (car lst-zones) (search (cdr lst-zones) frec)))
    (warm-up(low high) (if (and (>= frec low) (<= frec high)) (car lst-zones) (search (cdr lst-zones) frec)))
    (fat-burning(low high) (if (and (>= frec low) (<= frec high)) (car lst-zones) (search (cdr lst-zones) frec)))
    (aerobic(low high) (if (and (>= frec low) (<= frec high)) (car lst-zones) (search (cdr lst-zones) frec)))
    (anaerobic(low high) (if (and (>= frec low) (<= frec high)) (car lst-zones) (search (cdr lst-zones) frec)))
    (maximum(low high) (if (and (>= frec low) (<= frec high)) (car lst-zones) (search (cdr lst-zones) frec)))])


;Auxiliary Function
;I'm trying to find a number in a list of zones. If the list is empty zone sends an error
;Always return "No element"
(define (search my-zones num)
    (cond
      [(empty? my-zones) (error "No element")]
      [(equal? num (car my-zones)) (car my-zones)]
      [else (search (cdr my-zones) num)]))
    


;For this function, we'll recibe a tree of type BTree, we check if tree is empty and if this happen, we return a empty list.
;if not, we call to the function append of Racket, we'll take the element where we are and we'll call recursively to the function
;with the left and right subtrees, because the preorder in a tree is in this way: root, left child and right child 
;such that the childs are the sub-trees.

(define (preorderBT arbol)
  (type-case BTree arbol
    [EmptyBT () '()]
    [BNode (c l e r)
           (append (list e)(preorderBT l)(preorderBT r))]))
;test
(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))

;The inorderBT function does this: if tree is empty, we'll return a empty list
;if not, we'll use the append function with this elements, first we'll call recursively to the function with left subtree,
;we'll add the element root and again we'll call recursively to the function with right subtree.
;This format is because inorder in a tree is with left subtree, root and right subtree.
(define (inorderBT arbol)
  (type-case BTree arbol
    [EmptyBT () '()]
    [BNode (c l e r)
           (append (inorderBT l) (list e) (inorderBT r))]))
;test
(test (inorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))

;The posorderBT function works in this way:
;If tree is empty, we'll return a empty list;
;If not, we'll call recursively to the function with the left and right trees and we'll append to the root in a list;
;This format is because posorder is: left subtree, right subtree and root 
(define (posorderBT arbol)
  (type-case BTree arbol
    [EmptyBT () '()]
    [BNode (c l e r)
           (append (posorderBT l) (posorderBT r) (list e))]))
    
(test (posorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))


;To this function, we'll receive a tree:
;If tree is empty, we'll return zero.
;If not,we'll ask if left or right tree isn't empty. If this happen,  we'll checkout with the same trees because we'll know 
;that node isn't a leaf. In otherwise, we'll add 1.
(define (nlBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (cond
             [(not (or (EmptyBT? l) (EmptyBT? r))) (+ (nlBT l) (nlBT r))]
             [else 1])]))
;test
(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (EmptyBT) 9 (EmptyBT))) 1)
(test (nlBT (BNode < (BNode < ( BNode < (EmptyBT) 4 (EmptyBT))6 (BNode < (EmptyBT) 5 (EmptyBT))) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nlBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 5 (EmptyBT)) 8 (EmptyBT)) 10 (EmptyBT)) 1 (EmptyBT)))1)
(test (nlBT (BNode < (EmptyBT) 18 (BNode < (EmptyBT) 20 (EmptyBT)))) 1)



;To this function, we'll receive a tree:
;If tree is empty, we'll return zero. If not, we'll know that at least we have a node and we'll add 1 and then we'll call
;the elements of left subtree and right subtree with a sum.
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

;To this function, we'll receive a tree:
;If tree is empty, we'll return zero.
;If not,we'll ask if this element is a leaf, and for this we'll check if leaf doesn't have a left and right subtree and
;we won't add nothing. Otherwise we'll know that that isn't a leaf and we'll add 1 and then we'll call
;the elements of left subtree and right subtree with a sum.

(define (ninBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (cond
             [(and (EmptyBT? l) (EmptyBT? r)) 0]
             [else (+ 1 (+ (ninBT l) (ninBT r)))])]))
;test
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (EmptyBT) 1 (EmptyBT))) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT (BNode < (BNode < (EmptyBT) 7 (EmptyBT)) 10 (EmptyBT))) 1)
(test (ninBT (BNode < (BNode < ( BNode < (EmptyBT) 5 (EmptyBT))4 (BNode < (EmptyBT) 9 (EmptyBT))) 12 (BNode < (EmptyBT) 6 (EmptyBT)))) 2)


;For this, we'll receive two parameters, a function and a tree
;If tree is empty, we'll return a list with a EmptyBT element.
;If not, we'll checkout if that node is a leaf, and if this happen, we'll return the node with their elements, such as 
; the procedure, the leaf subtree, the application to the element and the right subtree;
;If that node isn't a leaf, we'll return a node with its procedure and we'll call recursively to the function with parameters
; function and leaf tree, and we'll apply the function to the element of that node and again we'll call recursively
; to the function mapBT with its function and right subtree.
(define (mapBT fun arbol)
  (type-case BTree arbol
    [EmptyBT () (EmptyBT)]
    [BNode (c l e r)
           (cond 
             [(and (EmptyBT? l) (EmptyBT? r)) (BNode c l (fun e) r)]
             [else (BNode c (mapBT fun l) (fun e) (mapBT fun r))])]))

(test (mapBT sub1 (BNode < (EmptyBT) 3 (EmptyBT))) (BNode < (EmptyBT) 2 (EmptyBT)))
(test (mapBT add1 (EmptyBT))(EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 5 (BNode < (EmptyBT) 7 (EmptyBT)))) (BNode < (EmptyBT) 6 (BNode < (EmptyBT) 8 (EmptyBT))))
(test (mapBT sub1 (BNode < (EmptyBT) 3 (EmptyBT))) (BNode < (EmptyBT) 2 (EmptyBT)))
(test (mapBT sub1 (BNode < (EmptyBT) 5 (BNode < (EmptyBT) 7 (EmptyBT)))) (BNode < (EmptyBT) 4 (BNode < (EmptyBT) 6 (EmptyBT))))

