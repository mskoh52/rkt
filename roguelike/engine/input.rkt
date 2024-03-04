#lang racket/base

(provide update-state)

(require
 racket/match
 "actions/move.rkt")

(define (update-state state key)
  (match key
    [#\k (hash-set state 'player (move (hash-ref state 'player) 'n))]
    [#\u (hash-set state 'player (move (hash-ref state 'player) 'ne))]
    [#\l (hash-set state 'player (move (hash-ref state 'player) 'e))]
    [#\m (hash-set state 'player (move (hash-ref state 'player) 'se))]
    [#\j (hash-set state 'player (move (hash-ref state 'player) 's))]
    [#\n (hash-set state 'player (move (hash-ref state 'player) 'sw))]
    [#\h (hash-set state 'player (move (hash-ref state 'player) 'w))]
    [#\y (hash-set state 'player (move (hash-ref state 'player) 'nw))]
    [_ state]))
