#lang racket

(require srfi/1)
(require srfi/13)



( define objects '((1 "a silver dagger")
                   (1 "a gold coin " )
                   (2 "a torch")
                   (2 "a laptop")
                   (3 "a gun")
                   (3 "a gold coin")
                   (4 "a dagger")
                   (4 "health booster")
                   (6 "sliver coin")
                   (6 "gun")))
                   


(define descriptions '( (1 "You are in the Entrance you can see an exit to the North and East.
You can see a silver dagger and a gold coin")
                        (2 "You are in the hallway there is an exit to the South and East.
You can see a Torch and Sparrow") 
                        (3 "You are in the garden there is an exit to the East and West.
You can see a gun a gold coin")
                        (4 "You are in the Corridor there is exit to the West and South.
You can see a health booster and a daggger")                   
                        (5 "You are in the bedroom there is exit to the West and North")
                        (6 "You are in the bathroom there is exit to the West.
You can see a sliver coin and gun")))
;(define directions '( (1 (north 2) (south 0) (east 0) (west 0))
;                      (2 (north 0) (south 1) (east 3) (west 0))
;                      (3 (north 0) (south 0) (east 4) (west 2))
;                      (4 (north 0) (south 5) (east 0) (west 3))
;                      (5 (north 0) (south 0) (east 0) (west 6)
;                      (6 (north 0) (south 0) (east 5) (west 1)))))

(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remov ) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))

(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))
(define decisiontable `((1 ((north) 2) ((east) 6) ,@actions)
                        (2 ((south) 1) ((east) 3) ,@actions)
                        (3 ((east)  4) ((west) 2) ,@actions)
                        (4 ((south) 5) ((west) 3) ,@actions)
                        (5 ((west)  6) ((east) 4) ,@actions)
                        (6 ((east)  5) ((west) 1) ,@actions)))


; ; the structure representing a maze of size NxM
( struct maze ( N M tbl ))
; ; managing cell properties
; ; A dictionary is an instance of a datatype that maps keys to values
; ; e . g . hash table , list , structures
( define ( connections tbl c) ( dict-ref tbl c '()))
; ; dict-set ! maps key to v in dict , overwriting any existing mapping for key
( define ( connect! tbl c n )
   ( dict-set! tbl c ( cons n ( connections tbl c )))
   ( dict-set! tbl n ( cons c ( connections tbl n ))))
( define ( connected? tbl a b) ( member a ( connections tbl b )))





; ; Returns a maze of a given size
; ; build-maze :: Index Index - > Maze
(define (build-maze N M)
  (define tbl (make-hash))
  (define (visited? tbl c) (dict-has-key? tbl c))
  (define (neigbours c)
    (filter
     (match-lambda [(list i j) (and (<= 0 i (- N 1)) (<= 0 j (- M 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))
  ;generate the maze
  (let move-to-cell ([c (list (random N) (random M ))])
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      (connect! tbl c n)
      (move-to-cell n)))
  ;return the result
  (maze N M tbl))





; ; ~~~ Users config ~~~
(define X 3)
(define Y 3)
(define start '(0 0))
; ; include maze algorithm with X and Y as M and N .
(define m (build-maze X Y))
; ; the paths function provides the available directions
(define (paths start)
  (match-define (maze N M tbl) m)
  (map (lambda (x)
         (let ((first (map = start x))
               (second (map < start x)))
           (cond [(car first)
                  (if (cadr second)  'south 'north )]
                 [else
                  (if (car second ) 'east  'west )]) ))
       (connections tbl start )))





( define room-type '( ( 0 "entrance
 You can see a Torch and Sparrow")
                      (1 "hallway")
                      (2 "garden ")
                      (3 "corridor " )
                      (4 "bedroom") 
                      (5 "bathroom" )
                      ))
                   
( define ( assq-ref assqlist id )
   ( cadr ( assq id assqlist )))
( define rooms ( make-hash ))
( define ( room-allocator db types )
   (for (( j X ))
     (for (( i Y ))
       (hash-set! db ( list j i) ( assq-ref types ( random (- ( length types ) 1)))))))
(room-allocator rooms room-type )




(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))



(define objectdb ( make-hash ))
(define inventorydb ( make-hash ))

(define (add-object db id object )
  (if (hash-has-key? db id )
      (let ((record (hash-ref db id )))
        (hash-set! db id (cons object record )))
      (hash-set! db id (cons object empty ))))

(define (add-objects db )
  (for-each
   (lambda (r)
     (add-object db (first r) (second r ))) objects))
(add-objects objectdb)



;(define (assq-ref assqlist id)
;(cdr (assq id assqlist)))

(define (get-room-description rid)
  (car (assq-ref descriptions rid)))
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))

(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))
;stop repetition 
(define (evaluate a b id)
  (cond  ((eq? a b )
          'bag)
         (else
          id)))
( define ( remove-object-from-room db id str )
   ;(define ( remove-object db id from input)
   ;(let* ((str (string-join (cdr (string-split input))));; remove car (pick)
   ;(newid (evaluate from 'bag id)))
   (when  (hash-has-key? db  )    (let* (( record ( hash-ref db  ))
                                         (result (remove (lambda (x) (string-suffix-ci? str x)) record))
                                         (item ( lset-difference equal? record result )))
                                    (cond (( null? item )
                                           (printf "I don ’t see that item in the room !\n"))
                                          ;            ( else
                                          ;              (cond ((eq? from 'room)
                                          ;              ( printf " Added ~a to your bag .\n" ( first item ))
              
                                          ( add-object inventorydb  'bag ( first item ))
                                          ( hash-set! db id result )))))
;            (else
;             (printf  "removed -a from your bag.\n" (first item))
;                      (add-object objectdb id (first item ))
;                                  (hash-set! db 'bag result)))))



(define (remove-object-from-inventory db id str )
  ( when ( hash-has-key? db 'bag )
     ( let*(( record ( hash-ref db 'bag ))
            ( result ( remove ( lambda (x) ( string-suffix-ci? str x )) record ))
            ( item ( lset-difference equal? record result )))
        ( cond (( null? item )
                ( printf " You are not carrying that item!\n" ))
               ( else
                 ( printf " Removed ~a from your bag .\n" (first item ))
                 ( add-object objectdb id ( first item ))
                 ( hash-set! db 'bag result ))))))



(define (pick-item  from id input)
  (if eq? from 'bag ))
;(remove-object inventorydb 'bag input)
; (remove-object objectdb id 'room input))


(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb  'bag))


(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id  'bag )
            (printf " You are carrying ~a .\n" output )
            (printf " You can see ~a .\n " output ))))))

;(define (lookup room-id direction)
; (car (assq-ref (assq-ref directions room-id) direction)))

;(define (lookup id tokens)
; (let* ((record (assv-ref decisiontable id))
;       (keylist (get-keywords id))
;      (index (index-of-largest-number (list-of-lengths keylist tokens))))
;(if index 
;   (cadr (list-ref record index))
;  #f)))

(define (get-response id)
  (car (assq-ref descriptions id)))



; ; show maze with position
(define (show-maze m pos )
  ( match-define ( maze X Y tbl ) m)
  ( for ([ i X ]) ( display "+---" ))
  ( displayln "+")
  ( for ([ j Y ])
     ( display "|")
     ( for ([ i (- X 0)])
        ( if ( equal? (list i j ) pos )
             ( display " *")
             ( display "  " ))
        ( if ( connected? tbl ( list i j ) ( list (+ 1 i) j ))
             ( display "  " )
             ( display " |" )))
     ( newline )
     ( for ([ i X ])
        ( if (connected? tbl ( list i j ) ( list i (+ j 1)))
             (display "+   ")
             (display "+---" )))
     ( displayln "+" )))



( define ( move-x room fun )
   ( cons ( car room ) ( map ( lambda ( x) ( fun x 1)) ( cdr room ))))

( define ( move-y room fun )
   
   ( cons ( fun ( car room ) 1) ( cdr room )))
( define (lookup room direction )
   ( cond [( eq? direction ' south )
           ( move-x room +)]
          [( eq? direction ' north )
           ( move-x room -)]
          [( eq? direction ' west )
           ( move-y room -)]
          [( eq? direction ' east )
           ( move-y room +)]))
;
;(define (startgame initial-id)
; (let loop ((id initial-id) (description #t))
;  (if description
;     (printf "~a\n> " (get-response id))
;    (printf "> "))
;(let* ((input (read-line))
;      (string-tokens (string-tokenize input))
;    (tokens (map string->symbol string-tokens)))
;(let ((response (lookup id tokens)))
;(cond ((number? response)
;      (loop response #t))
;    ((eq? #f response)
;    (format #t "huh? I didn't understand that!\n")
;   (loop id #f))
; ((eq? response 'look)
; (get-directions id)
;(loop id #f))
;((eq? response 'put)
; (put-item id input)
;(loop id #f))
;((eq? response 'pick)
;(pick-item id input)
;(loop id #f))
;((eq? response 'inventory)
;(display-inventory )
;(loop id #f))
;              
;((eq? response 'quit)
; (format #t "So Long, and Thanks for All the Fish...\n")
;              (exit)))))))


( define ( startgame room-id )
   ( let loop (( rid room-id ))
      (show-maze m rid )
      ( printf " You are in the ~a\n > " (hash-ref rooms rid ))
      ( let (( input ( read )))
         ( cond [( eq? input 'quit ) ( exit )]) ; ; ’ help with paths
         ( cond [( eq? input 'pick )  (pick-item display-objects objectdb  rid ) (loop rid)]) ; ; ’ help with paths
      

         ( if ( member input ( paths rid ))
              ( let (( direction ( lookup rid input )))
                 ( cond (( equal? rid direction ) ( loop rid ))
                        (( equal? direction ( list (- X 1)(- Y 1)))
                         ( show-maze m direction )
                         ( displayln " You have reached the exit door .")
                         ( exit ))

                        ( else
                          ( loop direction ))))
              ( begin
                 ( printf "huh? I didn ’t understand : ~a\n" input )
        ( loop rid ))))))




;(define (startgame room-id)
;  (let loop ((rid room-id))
;    (printf "~a\n" (get-room-description rid))
;    (printf "> ")
;    (let ((input (read)))
;      (if (eq? input 'quit) (exit) 'continue)
;      (if (member input '(north south east west))
;          (let ((direction (lookup rid input)))
;            (if (zero? direction)
;                (loop rid)
;                (loop direction)))
;          (begin
;            (printf "huh? I didn't understand: ~a\n" input)
;            (loop rid))))))

;(startgame  1)
(startgame start)