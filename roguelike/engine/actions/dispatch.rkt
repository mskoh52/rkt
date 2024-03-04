#lang racket/base

(provide
 dispatch-action)

(require
 racket/control
 racket/match
 nested-hash
 "move.rkt")

(define (dispatch-action action)
  (match action
    ['exit (lambda (state) (abort))]
    [(cons 'move (cons actor-key direction)) (lambda (state) (move state action))]
    [_ (lambda (state) state)]))
