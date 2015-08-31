#lang plai

(define (average lst)
  (cond
    [(empty? lst) 0]
    [(empty? (cdr lst))  (/ (+ (car lst) (average (cdr lst))) (mlength lst)) ]
    [else (+ (car lst) (average(cdr lst)))]))


    ;[else (/ (+ (car lst) (average (cdr lst))) (mlength lst))])) ;No esta funcionando, no devuelve los resultados que deberia

(test(average '()) 0)
(test(average '(1 2 3)) 2)
;(test(average '(18)) 18)


;Metodo auxiliar que saca la longitud de la lista y nos ayudara para sacar el promedio
(define (mlength a-lst)                   
  (cond                                   
    [(empty? a-lst) 0]                    
    [else (+ 1 (mlength (cdr a-lst)))]))  
(test (mlength '()) 0)
(test (mlength '(1 2 3 4)) 4)
(test (mlength '(5 6)) 2)

        
;(define (primes num)

;Siempre devuelve '()
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
