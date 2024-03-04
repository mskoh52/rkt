#lang racket/base

(provide handle-input)

(require
 racket/match)

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
