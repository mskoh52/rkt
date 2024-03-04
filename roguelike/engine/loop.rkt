#lang racket/base

(provide make-loop)

(require
 "input.rkt"
 "actions/dispatch.rkt")

(define (should-read-key? state)
  #t)

(define (make-loop draw! read-key)
  (define (loop state)
    (draw! state)
    (loop
     (if (should-read-key? state)
         ((dispatch-action (handle-input (read-key))) state)
         ;; TODO  handle npc actions instead of returning state unchanged
         state)))
  loop)
