#lang plai
; Metodo powerset
(define (powerset lst)
  (cond
    [(empty? lst) '(())]
    [else('(cons(car lst)) powerset(cdr lst))]))

(test (powerset '()) '(()))
;(test (powerset '(1 2)) '((1 2)(1)(2)()))

;;;;;;;;;;;;;;;;;;;;;;;;No funciona;;;;;;;;;;;;;;;;;;;;;;;;
(define (reduce fun lst)
  (cond
  [(empty? lst) '()]  
  [(equal?(procedure? fun) #f) '()]
  [else (cons(fun (car lst))
          (reduce fun (cdr lst)))]))


;;;;;;;;;;Función pow;;;;;;;;;;

(define (pow n m)
  (cond
   [(equal? n 0) 0]
   [(equal? m 0) 1]
   [else (* n (pow n (- m 1)))]))
(test(pow 5 3) 125)
(test(pow 4 6) 4096)
(test(pow 20 0) 1)
(test(pow 0 5) 0)
(test(pow 100 2) 10000)



;;;;;;;;;;Function primes;;;;;;;;;
;By The function call is prime, we know if the number That We passed as parameters is prime, if this happens, we add this number 
;an list, but if this does not happen we continue with the process. We subtract 1 from ours a parameter and now see if that 
;number is prime. We do this by recursively calling this method.Once we have all the primes ​​from 2 to the number we were given 
;as a parameter, we return the list of all prime numbers.
;If the number that happen to us is 0 or 1 return the empty list since those numbers are not prime and are not above 2.

(define (primes n)
  (cond
    [(or(equal? n 0) (equal? n 1)) '()]
    [(not (esprimo n)) (primes (sub1 n))]
    [else (cons n (primes (sub1 n)))]))


;;;;;Auxiliar function divided;;;;;
;Divide the first number that happen to us as a parameter between the second and if it does not an integer take the floor of 
;that number, then multiply that result by the second number we have as parameter (it was the same by which divide the first 
;number) and we verify if that result gives us the number we divided,if this happens it means that the second number is a divisor
;of the first number and return 1, otherwise return 0

(define (divide num1 num2)
  (if ( = num1 ( * (floor (/ num1 num2)) num2))1 0))


;;;;;Auxiliar function to count the number of dividers;;;;;
;Call the function split to see if a number is a divisor of another , and if so will increase by 1 the counter of dividers, which
;starts at zero, otherwise keep looking dividers number.

(define (divisores num div)
    (if (= div 0)
        0
   (+ (divide num div)(divisores num(- div 1)))))
   
   
;;;;;Auxiliar function to determine if a number is prime or not;;;;;;
;This get the total number of dividers that has a number, through the call to the function dividers, if the number has only two 
;dividers then will return #t , indicating that the number is prime, otherwise it will return #f

(define (esprimo n)
(if (= (divisores n n) 2) #t #f))


;;;;;Testing;;;;;
(test(primes 11) '(11 7 5 3 2))
(test(primes 30) '(29 23 19 17 13 11 7 5 3 2))
(test(primes 0) '())
(test(primes 19) '(19 17 13 11 7 5 3 2))
(test(primes 1) '())


;;;;;;;;;;zip Function;;;;;;;;;
;In this function we going to have a list of sub-lists from two lists that happen to us as a parameter.The first thing to do is 
;verify that none of the lists that happen to us is empty, if there is empty then return an empty list because we can not attach
;an empty list with another list. If we are not empty lists, we will form a list with a sublist with the head of each of the
;lists be upgraded and re-call this method to do the same again with the rest of the lists.

(define (zip l1 l2)
  (cond
    [(or(empty? l1) (empty? l2)) '()]
    [else 
      (cons(list(car l1) (car l2))
           (zip (cdr l1) (cdr l2)))]))
           
           
;;;;;Testing;;;;;
(test(zip '(1 2) '(3 4)) '((1 3) (2 4)))
(test(zip '() '(20 40)) '())
(test(zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
(test(zip '(7 9) '(15 21 25 37)) '((7 15) (9 21)))
(test(zip '(50 100 150) '()) '())

  
;Metodo every?
(define (every? pred lst)
  (cond
    [(empty? lst) #t]
    [(not(pred (car lst))) #f]
    [else (every? pred (cdr lst))]))

(test (every? number? '()) #t)
(test (every? positive? '(1 43 2 21)) #t)
(test (every? number? '(1 2 y 344)) #f)
(test (every? symbol? '(a a a -4)) #f)
(test (every? symbol? '(a d f j l)) #t)

;Metodo any?
(define (any? pred lst)
  (cond
    [(empty? lst) #f]
    [(pred (car lst)) #t]
    [else (any? pred (cdr lst))]))

(test (any? positive? '()) #f)
(test (any? number? '(a 43 "agg")) #t)
(test (any? string? '(1 2 42 344)) #f)
(test (any? positive? '(78 a 781 -4)) #t)
(test (any? symbol? '(90 10 18 a)) #t)

; Obtener el promedio.
(define (average lst)
   (cond 
     [(empty? lst) 0]
     [else (/ (suma lst) (mlength lst))]))
 
(define (suma lst)
   (cond
    [(empty? lst) 0]
    [else (+ (car lst) (suma(cdr lst)))]))

(define (mlength a-lst)                   
  (cond                                   
    [(empty? a-lst) 0]                    
    [else (+ 1 (mlength (cdr a-lst)))]))  

(test (average '(7 8)) 7.5)
(test (average '()) 0)
(test (average '(10 10 10 10 10)) 10)
(test (average '(1 2 3)) 2)
(test (average '(10 14 19 5)) 12) 


;;;;;mconcat function;;;;;
;We will join two lists that happen to us as a parameter .
;If either of the two lists that happen to us is empty, we return only the other list. If either list is empty, we create a list
;with the head of the list and return to first call the function to continue to get the rest of the elements of the first list
;and the second and added to the new list.

(define (mconcat lst1 lst2)
  (cond
   [(empty? lst1) lst2]
   [(empty? lst2) lst1]
   [else (cons(car lst1) (mconcat (cdr lst1) lst2))]))
   
   
;;;;;Testing;;;;;
(test (mconcat '(10 20 30) '(40)) '(10 20 30 40))
(test (mconcat '() '(2 4 6)) '(2 4 6))
(test (mconcat '(8 9) '()) '(8 9))
(test (mconcat '() '()) '())
(test (mconcat '() '(1 2 4 5)) '(1 2 4 5))


;;;;;;;;;;mmap function;;;;;;;;;;
;First we have to verify that the parameter that is expected to be a function really is, if not a function there will be nothing
;you can do and then return the empty list.
;If we get what we expect (a function), then we cross the list to apply that function to every element of the list we received as
;parameter. To do this we created a new list and save the head of the list to be upgraded after applying the function. We again 
;call this function to do the same with the rest of the list.

(define (mmap fun lst)
  (cond
    [(empty? lst) '()]
    [(equal?(procedure? fun) #f) '()]
    [else (cons(fun(car lst))
          (mmap fun (cdr lst)))]))

;;;;;Testing;;;;;
(test (mmap add1 '(0 1 2 3 4 5 6 7 8 9)) '(1 2 3 4 5 6 7 8 9 10))
(test (mmap 56 '(2 3 4)) '())
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))
(test (mmap #f '(50 40 30 20 10)) '())

