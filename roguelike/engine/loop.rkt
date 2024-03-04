#lang racket/base

(provide make-loop)

(require "input.rkt")

(define (make-loop draw! read-key)
  (define (loop state)
    (draw! state)
    (let ([key (read-key)])
      (if (eq? key 'ctrl-q)
          (exit)
          (loop (update-state state key)))))
  loop)
