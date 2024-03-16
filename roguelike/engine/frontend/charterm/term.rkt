#lang racket/base

(provide main)

(require racket/pretty
         "charterm.rkt"
         "input.rkt"
         "../../loop.rkt")

(define (level-to-str level)
  (map (lambda (row) (list->string (map (lambda (t) (hash-ref (hash-ref t 'def) 'glyph)) row)))
       (hash-ref level 'terrain)))

(define (draw-level! level)
  (draw-lines! (level-to-str level)))

(define (draw-lines! lines)
  (for ([line lines])
    (charterm-display line)
    (charterm-newline)))

(define (draw-actor! actor)
  (let ([pos (hash-ref actor 'pos)])
    (charterm-cursor (+ 1 (car pos)) (+ 1 (cdr pos)))
    (charterm-display (hash-ref actor 'glyph))))

(define (draw-state! state)
  (charterm-cursor 1 32)
  (charterm-display (pretty-format state)))

(define (draw! state)
  (charterm-cursor 1 1)
  (draw-level! (hash-ref state 'level))
  (for ([actor (hash-map (hash-ref state 'actors) (lambda (k v) v))])
    (draw-actor! actor)))

(define (main state)
  (with-charterm
   (charterm-clear-screen)
   ((make-loop draw! (make-get-action (+ 2 (length (hash-ref (hash-ref state 'level) 'terrain)))))
    state)))
