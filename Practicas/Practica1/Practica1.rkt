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



;;;;;;;;;;Función Primes;;;;;;;;;

(define (primes n)
  (cond
    [(or(equal? n 0) (equal? n 1)) '()]
    [(not (esprimo n)) (primes (sub1 n))]
    [else (cons n (primes (sub1 n)))]))

  
(define (divide num1 num2)
  (if ( = num1 ( * (floor (/ num1 num2)) num2))1 0))


(define (divisores num div)
    (if (= div 0)
        0
   (+ (divide num div)(divisores num(- div 1)))))

(define (esprimo n)
(if (= (divisores n n) 2) #t #f))

(test(primes 11) '(11 7 5 3 2))
(test(primes 30) '(29 23 19 17 13 11 7 5 3 2))
(test(primes 0) '())
(test(primes 19) '(19 17 13 11 7 5 3 2))
(test(primes 1) '())


;;;;;;;;;;Función zip;;;;;;;;;

(define (zip l1 l2)
  (cond
    [(or(empty? l1) (empty? l2)) '()]
    [else 
      (cons(list(car l1) (car l2))
           (zip (cdr l1) (cdr l2)))]))
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

;Metodo concatenar

(define (mconcat lst1 lst2)
  (cond
   [(empty? lst1) lst2]
   [(empty? lst2) lst1]
   [else (cons(car lst1) (mconcat (cdr lst1) lst2))]))
         
(test (mconcat '(10 20 30) '(40)) '(10 20 30 40))
(test (mconcat '() '(2 4 6)) '(2 4 6))
(test (mconcat '(8 9) '()) '(8 9))
(test (mconcat '() '()) '())
(test (mconcat '() '(1 2 4 5)) '(1 2 4 5))

;(define (primes num);Siempre devuelve '()
(define (zip l1 l2)
  (cond
    [(empty? l1) '()]
    [(empty? l2) '()]
    [else 
     (define p (cons(car l1) (car l2)))
     (zip (cdr l1) (cdr l2))]))
;(test(zip '(1 2) '(3 4)) '(1 3) '(2 4))

  
