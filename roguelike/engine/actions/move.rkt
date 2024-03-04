#lang racket/base

(provide move)

(require
 racket/match)

(module+ test (require rackunit))

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

(module+ test
  (define p '(0 . 0))
  (test-case
   "move-pos"
   (check-equal? (move-pos p 'n ) '( 0 . -1 ) "n" )
   (check-equal? (move-pos p 'ne) '( 1 . -1) "ne")
   (check-equal? (move-pos p 'e ) '( 1 .  0) "e" )
   (check-equal? (move-pos p 'se) '( 1 .  1) "se")
   (check-equal? (move-pos p 's ) '( 0 .  1) "s" )
   (check-equal? (move-pos p 'sw) '(-1 .  1) "sw")
   (check-equal? (move-pos p 'w ) '(-1 .  0) "w" )
   (check-equal? (move-pos p 'nw) '(-1 . -1) "nw")))

(define (move actor direction)
  (hash-set actor 'pos (move-pos (hash-ref actor 'pos) direction)))

(module+ test
  ;; (define state
  ;;   (hash 'actors (hash 'pc (hash 'pos p))
  ;;         'level '(TODO)))
  (define actor (hash 'pos p))
  (test-case
   "move-pos"
   (check-equal? (hash-ref (move actor 'n ) 'pos) '( 0 . -1) "n" )
   (check-equal? (hash-ref (move actor 'ne) 'pos) '( 1 . -1) "ne")
   (check-equal? (hash-ref (move actor 'e ) 'pos) '( 1 .  0) "e" )
   (check-equal? (hash-ref (move actor 'se) 'pos) '( 1 .  1) "se")
   (check-equal? (hash-ref (move actor 's ) 'pos) '( 0 .  1) "s" )
   (check-equal? (hash-ref (move actor 'sw) 'pos) '(-1 .  1) "sw")
   (check-equal? (hash-ref (move actor 'w ) 'pos) '(-1 .  0) "w" )
   (check-equal? (hash-ref (move actor 'nw) 'pos) '(-1 . -1) "nw")))
