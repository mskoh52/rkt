#lang racket/base

(provide move)

(require
 racket/match)

(define (move-pos pos direction)
  (match direction
    ['n  (cons (+ (car pos)  0) (+ (cdr pos) -1))]
    ['ne (cons (+ (car pos)  1) (+ (cdr pos) -1))]
    ['e  (cons (+ (car pos)  1) (+ (cdr pos)  0))]
    ['se (cons (+ (car pos)  1) (+ (cdr pos)  1))]
    ['s  (cons (+ (car pos)  0) (+ (cdr pos)  1))]
    ['sw (cons (+ (car pos) -1) (+ (cdr pos)  1))]
    ['w  (cons (+ (car pos) -1) (+ (cdr pos)  0))]
    ['nw (cons (+ (car pos) -1) (+ (cdr pos) -1))]))

(define (move actor direction)
  (hash-set actor 'pos (move-pos (hash-ref actor 'pos) direction)))
