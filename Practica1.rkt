#lang plai

(define (pow n m)
  (cond
   [(equal? m 0) 1]
   [else (* n (pow n (- m 1)))]))

(test(pow 5 3) 125)
(test(pow 4 6) 4096)
(test(pow 20 0) 1)



(define (average lst)
  (cond
    [(empty? lst) 0]
    [else(+ (car lst) (average (cdr lst)))]))
(test(average '(1 2 3)) 6)
(test(average '(4 7 9 10)) 30)
(test(average '(18)) 18)


        
;(define (primes num)


;(define (zip l1 l2)
;  (cond
;    [(empty? l1) '()]
;    [(empty? l2) '()]
;   [else (cons(car l1) (zip (cons(car l2)) (cdr l1)))]))
;(test(zip '(1 2) '(3 4)) '(1 3) '(2 4))
