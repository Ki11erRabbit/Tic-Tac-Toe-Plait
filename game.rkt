#lang plait


;Because PLAIT is unable to infer the type of a list when an empty list is created with empty, this function
;creates an empty list by creating a list with one element and then dropping that element.
(define (empty-list a)
  (drop 1 (build-list 1 (lambda (x) a))))

(define (take [n : Number] [lst : (Listof 'a)]) : (Listof 'a)
  (if (> n 0)
      (cons (first lst)
            (take (- n 1) (rest lst)))
        empty))
(define (drop [n : Number] [lst : (Listof 'a)]) : (Listof 'a)
  (if (> n 0)
      (drop (- n 1) (rest lst))
      lst))

(define (flatten [lst : (Listof (Listof 'a))]) : (Listof 'a)
  (cond
    [(empty? lst) empty]
    [else (append (first lst) (flatten (rest lst)))]))

(define (rotate-grid-90 [grid : (Listof (Listof 'a))]) : (Listof (Listof 'a))
  (local [(define flat (flatten grid))]
          (list (list (list-ref flat 2) (list-ref flat 6) (list-ref flat 10) (list-ref flat 3))
                (list (list-ref flat 1) (list-ref flat 5) (list-ref flat 9) (list-ref flat 7))
                (list (list-ref flat 0) (list-ref flat 4) (list-ref flat 8) (list-ref flat 11)))))

(define (align-diagonals [grid : (Listof (Listof 'a))]) : (Listof (Listof 'a))
  (local [(define flat (flatten grid))]
    (list (list (list-ref flat 2) (list-ref flat 5) (list-ref flat 8) (list-ref flat 3))
          (list (list-ref flat 0) (list-ref flat 5) (list-ref flat 10) (list-ref flat 7)))))



(define-type Coord
  (cord [x : Number] [y : Number]))


;I would define the type Board to be a list of list of characters
;but plait doesn't like nested lists as a type 
(define (draw-board board)
  (map2 (lambda (row vd) row) board (map print-row board)))

(define (print-row [row : (Listof Char)])
  (map display row))


(define (create-board)
  (list (list #\. #\. #\. #\newline) (list #\. #\. #\. #\newline) (list #\. #\. #\. #\newline)))

(define (mark-board board coord mark)
    (let ([row (cord-x coord)]
            [col (cord-y coord)])
        (let ([new-row (take col (list-ref board row) )])
        (let ([new-row (append new-row (list mark))])
            (let ([new-row (append new-row (drop (+ col 1) (list-ref board row)))])
            (let ([new-board (take row board)])
                (let ([new-board (append new-board (list new-row))])
                (let ([new-board (append new-board (drop (+ row 1) board))])
                  new-board))))))))

(define (check-full board)
  (not (member #\. (flatten board))))

(define (check-win board mark)
    (or (check-win-row board mark)
        (check-win-row (rotate-grid-90 board) mark)
        (check-win-row (align-diagonals board) mark)))

(define (check-win-row board mark)
  (member #t (map (lambda (row) (check-win-row-helper row mark)) board)))

(define (check-win-row-helper row mark) : Boolean
  (if (= (foldl (lambda (x y) 
                                 (if (char=? mark x)
                                  (+ 1 y)
                                  y)) 0 row) 3)
          #t
          #f))


(define (parse [s : S-Exp]) : Coord
  (let ([lst (s-exp->list s)])
    (let ([x (s-exp->number (first lst))])
      (let ([y (s-exp->number (second lst))])
        (if (or (> x 2) (< x 0) (> y 2) (< y 0))
            (begin
              (display "Invalid input, try again: ")
              (parse (read)))
            (cord x y))))))

(define (gen-cord moves) : Coord
  (let ([move (parse (read))])
    (if (member move moves)
        (begin
          (display "Invalid input, try again: ")
          (gen-cord moves))
        move)))


(define (play-turn board moves mark other-mark ai)
  ;(let ([temp (draw-board board)])
  (let ([coord (gen-cord moves)])
    (let ([moves (append moves (list coord))])
    (let ([new-board (mark-board board coord mark)])
      (if (check-win new-board mark)
          (begin
            (display "Player ")
            (display mark)
            (display " wins!")
            (display "\n")
            (display "Final board:")
            (display "\n")
            (draw-board new-board))
          (if (check-full new-board)
              (begin
                (display "Draw!")
                (display "\n")
                (display "Final board:")
                (display "\n")
                (draw-board new-board))
              (begin
                (display "Next turn:")
                (display "\n")
                (draw-board new-board)
                (if ai
                    (play-ai-turn new-board moves other-mark mark)
                    (play-turn new-board moves other-mark mark ai)))))))))


(define (play-ai-turn board moves mark other-mark)
  (let ([coord (ai-move moves board mark other-mark)])
    (let ([moves (append moves (list coord))])
    (let ([new-board (mark-board board coord mark)])
      (if (check-win new-board mark)
          (begin
            (display "Player ")
            (display mark)
            (display " wins!")
            (display "\n")
            (display "Final board:")
            (display "\n")
            (draw-board new-board))
          (if (check-full new-board)
              (begin
                (display "Draw!")
                (display "\n")
                (display "Final board:")
                (display "\n")
                (draw-board new-board))
              (begin
                (display "Next turn:")
                (display "\n")
                (draw-board new-board)
                (play-turn new-board moves other-mark mark #t))))))))

(define (ai-move moves board mark other-mark) : Coord
  (let ([bestVal (move (cord -1 -1) -1000)])
      (move-coord (foldl (lambda (i bestValue) (foldl (lambda (j bstVal) (if (char=? (list-ref (list-ref board i) j) #\.)
                                                                 (let ([new-board (mark-board board (cord i j) mark)])
                                                                   (move-max bstVal (move (cord i j) (minimax new-board 0 #f mark other-mark))))
                                                                 bstVal)) bestValue (list 0 1 2))) bestVal (list 0 1 2)))))

  

(define-type Move
  (move [coord : Coord] [score : Number]))

(define (move-max [move1 : Move] [move2 : Move]) : Move
  (if (> (move-score move1) (move-score move2))
      move1
      move2))


(define (eval-board board mark other-mark)
  (if (check-win board mark)
      10
      (if (check-win board other-mark)
          -10
          0)))

(define (minimax board depth isMaximizingPlayer mark other-mark)
  (let ([score (eval-board board mark other-mark)])
    (if (= score 10)
        score
        (if (= score -10)
            score
            (if (check-full board)
                0
                (if isMaximizingPlayer
                    (let ([bestVal -1000])
                      (foldl (lambda (i bestValue) (foldl (lambda (j bstVal) (begin
                                                                              (let ([new-board (mark-board board (cord i j) mark)])
                                                                                (max bstVal (minimax new-board (+ depth 1) (not isMaximizingPlayer) mark other-mark)))))
                                                         bestValue (list 0 1 2))) bestVal (list 0 1 2)))
                    (let ([bestVal 1000])
                        (foldl (lambda (i bestValue) (foldl (lambda (j bstVal) (begin
                                                                                (let ([new-board (mark-board board (cord i j) other-mark)])
                                                                                    (min bstVal (minimax new-board (+ depth 1) (not isMaximizingPlayer) mark other-mark)))))
                                                             bestValue (list 0 1 2))) bestVal (list 0 1 2)))))))))
                      
                          


(define (start-two-player-game)
  (begin
    (display "Player X, enter your move as a pair of numbers like this (0 0): ")
    (play-turn (draw-board (create-board)) (empty-list (cord 0 0)) #\x #\o #f)))

(define (start-one-player-game)
  (begin
    (display "Player X, enter your move as a pair of numbers like this (0 0): ")
    (play-turn (draw-board (create-board)) (empty-list (cord 0 0)) #\x #\o #t)))
