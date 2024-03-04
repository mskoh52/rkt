#lang racket/base

(require
 charterm
 racket/match
 racket/list)

(struct Pos (x y))

(struct Character
  (pos))

(struct State
  (player))

(define (make-field x y)
  (define (make-inner-row x)
    (string-append "#" (make-string (- x 2) #\ ) "#"))
  (flatten
   (list
    (make-string x #\#)
    (for/list ([_ (- y 2)])
      (make-inner-row x))
    (make-string x #\#))))

(define (draw-lines! lines)
  (for ([line lines])
    (charterm-display line)
    (charterm-newline)))

(define (draw-character! character)
  (let ([pos (Character-pos character)])
    (charterm-cursor (Pos-x pos) (Pos-y pos))
    (charterm-display "@")))

(define (draw! state)
  (charterm-clear-screen)
  (draw-lines! (make-field 80 20))
  (charterm-cursor 10 10)
  (draw-character! (State-player state))
  (charterm-cursor 1 1))

(define (move-pos pos direction)
  (match direction
    ['n  (Pos (+ (Pos-x pos)  0) (+ (Pos-y pos) -1))]
    ['ne (Pos (+ (Pos-x pos)  1) (+ (Pos-y pos) -1))]
    ['e  (Pos (+ (Pos-x pos)  1) (+ (Pos-y pos)  0))]
    ['se (Pos (+ (Pos-x pos)  1) (+ (Pos-y pos)  1))]
    ['s  (Pos (+ (Pos-x pos)  0) (+ (Pos-y pos)  1))]
    ['sw (Pos (+ (Pos-x pos) -1) (+ (Pos-y pos)  1))]
    ['w  (Pos (+ (Pos-x pos) -1) (+ (Pos-y pos)  0))]
    ['nw (Pos (+ (Pos-x pos) -1) (+ (Pos-y pos) -1))]))

(define (move character direction)
  (struct-copy Character character [pos (move-pos (Character-pos character) direction)]))

(define (update-state state key)
  (match key
    [#\h (struct-copy State state [player (move (State-player state) 'w)])]
    [#\j (struct-copy State state [player (move (State-player state) 's)])]
    [#\k (struct-copy State state [player (move (State-player state) 'n)])]
    [#\l (struct-copy State state [player (move (State-player state) 'e)])]))

(define (loop state)
  (draw! state)
  (let ((key (charterm-read-key)))
    (if (eq? key #\q)
        (begin
          (charterm-clear-screen)
          (exit))
        (loop (update-state state key)))))


(with-charterm
 (loop (State (Character (Pos 10 10)))))
