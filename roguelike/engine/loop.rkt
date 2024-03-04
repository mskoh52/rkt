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
         state)))
  loop)
