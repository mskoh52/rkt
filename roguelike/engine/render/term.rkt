#lang racket/base

(require
 racket/pretty
 math/array
 "charterm.rkt"
 "../loop.rkt")

(provide main)

(define (level-to-str level)
  (map (lambda (row)
         (list->string (map (lambda (t) (hash-ref (hash-ref t 'def) 'glyph)) row)))
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
    (draw-actor! actor))
  (charterm-cursor 1 30)
)

(define (read-key)
  (let ([key (charterm-read-key)])
    (charterm-cursor 1 30)
    (charterm-clear-line-right)
    (charterm-display (if (char? key) (string key) (symbol->string key)))
    key))

(define (main state)
  (with-charterm
   (charterm-clear-screen)
   ((make-loop draw! read-key) state)))
