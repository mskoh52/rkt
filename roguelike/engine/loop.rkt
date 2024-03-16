#lang racket/base

(provide make-loop)

(require "actions/dispatch.rkt")

(define (should-get-action? state)
  #t)

(define (make-loop draw! get-action)
  (define (loop state)
    (draw! state)
    (loop (if (should-get-action? state)
              ((dispatch-action (get-action)) state)
              ;; TODO  handle npc actions instead of returning state unchanged
              state)))
  loop)
