#lang racket/base

(provide
 dispatch-action
 )

(require
 racket/control
 racket/match
 nested-hash
 "move.rkt")

(define (dispatch-action action)
  (match action
    ['exit (lambda (state) (abort))]
    [(cons 'move (cons actor-key direction))
     (lambda (state)
       (let ([actor (nested-hash-ref state 'actors actor-key)])
         (nested-hash-set state 'actors actor-key (move actor direction))))]
    [_ (lambda (state) state)]))
