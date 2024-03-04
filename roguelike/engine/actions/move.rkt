#lang racket/base

(provide move)

(require
 racket/match
 "../pos.rkt")

(define (move-pos pos direction)
  (match direction
    ['n  (Pos (+ (Pos-x pos)  0) (+ (Pos-y pos) -1))]
    ['ne (Pos (+ (Pos-x pos)  1) (+ (Pos-y pos) -1))]
    ['e  (Pos (+ (Pos-x pos)  1) (+ (Pos-y pos)  0))]
    ['se (Pos (+ (Pos-x pos)  1) (+ (Pos-y pos)  1))]
    ['s  (Pos (+ (Pos-x pos)  0) (+ (Pos-y pos)  1))]
    ['sw (Pos (+ (Pos-x pos) -1) (+ (Pos-y pos)  1))]
    ['w  (Pos (+ (Pos-x pos) -1) (+ (Pos-y pos)  0))]
    ['nw (Pos (+ (Pos-x pos) -1) (+ (Pos-y pos) -1))]))

(define (move character direction)
  (hash-set character 'pos (move-pos (hash-ref character 'pos) direction)))
