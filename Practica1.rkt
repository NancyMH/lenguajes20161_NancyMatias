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


(define (zip l1 l2)
  (cond
    [(empty? l1) '()]
    [(empty? l2) '()]
    [else 
     (define p (cons(car l1) (car l2)))
     (zip (cdr l1) (cdr l2))]))
;(test(zip '(1 2) '(3 4)) '(1 3) '(2 4))

  

(define (mconcat lst1 lst2)
  (cond
   [(empty? lst1) lst2]
   [(empty? lst2) lst1]
   [else (cons(car lst1) (mconcat (cdr lst1) lst2))]))
          
(test (mconcat '(10 20 30) '(40)) '(10 20 30 40))
(test (mconcat '() '(2 4 6)) '(2 4 6))
(test (mconcat '(8 9) '()) '(8 9))


