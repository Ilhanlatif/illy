#lang racket

 
 (define multirember
    ;the function takes two arguments
    (lambda (a lat) 
    (cond
           ;if lat is empty the value should be empty 
             ((null? lat) (quote ()))
             
                       (else 
                               (cond
                                     ;else check if 'a' is equal to to the first atom in the list 
                                          ((eq? (car lat) a)

                                     ;if it is equal then print out the rest of the list
                                                  ( multirember a (cdr lat)))
                                    
                                     ;otherwise add    (car lat)   to (multirember a (cdr lat) when there is no more occurrences but first..
                                                           (else (cons (car lat)
                                                        
                                     ;call the function again to check if there ae anymore recurrences
                                                                  ( multirember a
                                                                                (cdr lat)))))))))
;calling the function
(multirember 'book '(paper book pen book pencil ))
(multirember 'w '( w h o w l i d w a y))



(multirember 'cup '(coffee cup tea cup and hick cup) )
;the function cant find cup so it saves 'coffee' and keeps looking 
(cons 'coffee (multirember 'cup '(cup tea cup and hick cup)))
;the function finds 'cup' and removes it from  cdr lat continues to look for any more occurrence  
(cons 'coffee  (multirember 'cup '(tea cup and hick cup)))
;function cant find 'cup' so it saves 'tea' and keeps looking
(cons 'coffee (cons 'tea  (multirember 'cup '(cup and hick cup))))
;function finds 'cup' and removes it from cdr lat
(cons 'coffee (cons 'tea (cons 'and ( multirember 'cup '(hick cup)))))
;function cant find 'cup' so saves 'hick' and continues to look 
(cons 'coffee (cons 'tea (cons 'and (cons 'hick (multirember 'cup '(cup))))))
;the function removes 'cup' from the cdr lat and continues to look for any more occurrence 
(cons 'coffee (cons 'tea (cons 'and (cons 'hick (quote ())))))
; the function is constructing 'quote' to 'hick'
(cons 'coffee (cons 'tea (cons 'and (cons 'hick  '()))))
;function constructs the empty list to to (cons'tea cons 'and cons 'hick)
(cons 'coffee  '(tea and  hick ))
;function constructs coffe with the cdr lat


