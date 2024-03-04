#lang racket/base

(provide handle-input)

(require
 racket/match)

;; (define (update-state state key)
;;   (match key
;;     [#\k (hash-set state 'player (move (hash-ref state 'player) 'n))]
;;     [#\u (hash-set state 'player (move (hash-ref state 'player) 'ne))]
;;     [#\l (hash-set state 'player (move (hash-ref state 'player) 'e))]
;;     [#\m (hash-set state 'player (move (hash-ref state 'player) 'se))]
;;     [#\j (hash-set state 'player (move (hash-ref state 'player) 's))]
;;     [#\n (hash-set state 'player (move (hash-ref state 'player) 'sw))]
;;     [#\h (hash-set state 'player (move (hash-ref state 'player) 'w))]
;;     [#\y (hash-set state 'player (move (hash-ref state 'player) 'nw))]
;;     [_ state]))

(define (handle-input key)
  ;;; returns an action as defined in actions/dispatch.rkt
  (match key
    ['ctrl-q 'exit]
    [#\k '(move . (pc . n))]
    [#\u '(move . (pc . ne))]
    [#\l '(move . (pc . e))]
    [#\m '(move . (pc . se))]
    [#\j '(move . (pc . s))]
    [#\n '(move . (pc . sw))]
    [#\h '(move . (pc . w))]
    [#\y '(move . (pc . nw))]
    [_ '()]))
