#lang racket/base

(require
 math/array
 "charterm.rkt"
 "../structs.rkt"
 "../level.rkt")

(provide
 draw!)

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

(define (draw! state)
  (charterm-cursor 1 1)
  (draw-level! (hash-ref state 'level))
  (draw-character! (hash-ref state 'player))
  (charterm-cursor 1 1))
