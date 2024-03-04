#lang racket/base

(require
 math/array
 "charterm.rkt"
 "../level.rkt"
 "../loop.rkt"
 "../pos.rkt")

(provide main)

(define (level-to-str level)
  (map (lambda (row)
         (list->string (map (lambda (t) (TerrainDef-glyph (Terrain-def t))) row)))
       (Level-terrain level)))

(define (draw-level! level)
  (draw-lines! (level-to-str level)))

(define (draw-lines! lines)
  (for ([line lines])
    (charterm-display line)
    (charterm-newline)))

(define (draw-character! character)
  (let ([pos (hash-ref character 'pos)])
    (charterm-cursor (+ 1 (Pos-x pos)) (+ 1 (Pos-y pos)))
    (charterm-display "@")))

(define (draw-state! state)
  (charterm-cursor 1 32)
  (charterm-display state))

(define (draw! state)
  (charterm-cursor 1 1)
  (draw-level! (hash-ref state 'level))
  (for ([actor (hash-map (hash-ref state 'actors) (lambda (k v) v))])
    (draw-character! actor))
  (draw-state! state))

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
