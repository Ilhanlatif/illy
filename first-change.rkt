#lang racket

;; The first version of MUD game

(define descriptions '( (1 "You are in the lobby you can see an exit to the North.")
                        (2 "You are in the hallway there is an exit to the South and East") 
                        (3 "You are in the kitchen there is an exit to the east and west")
                        (4 "You are in the living room there is exit to the Wes and south")
                        (5 "You are in the bedroom there is exit to the West and north")
                        (6 "You are in the toilet room there is exit to the West")))
(define directions '( (1 (north 2) (south 0) (east 0) (west 0))
                      (2 (north 0) (south 1) (east 3) (west 0))
                      (3 (north 0) (south 0) (east 4) (west 2))
                      (4 (north 0) (south 5) (east 0) (west 3))
                      (5 (north 0) (south 0) (east 0) (west 6)
                      (6 (north 0) (south 0) (east 5) (west 1)))))
                                

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

(define (lookup room-id direction)
  (car (assq-ref (assq-ref directions room-id) direction)))

(define (startgame room-id)
  (let loop ((rid room-id))
    (printf "~a\n" (get-room-description rid))
    (printf "> ")
    (let ((input (read)))
      (if (eq? input 'quit) (exit) 'continue)
      (if (member input '(north south east west))
          (let ((direction (lookup rid input)))
            (if (zero? direction)
                (loop rid)
                (loop direction)))
          (begin
            (printf "huh? I didn't understand: ~a\n" input)
            (loop rid))))))

;(startgame 1)
