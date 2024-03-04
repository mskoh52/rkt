#lang racket/base

(provide move)

(require
 racket/match
 nested-hash
 "../level.rkt"
 "../state.rkt"
 "../combat/melee.rkt")

(module+ test
  (require
   racket/list
   rackunit))

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
  (define p '(1 . 1))
  (test-case
   "move-pos"
   (check-equal? (move-pos p 'n ) '(1 . 0) "n" )
   (check-equal? (move-pos p 'ne) '(2 . 0) "ne")
   (check-equal? (move-pos p 'e ) '(2 . 1) "e" )
   (check-equal? (move-pos p 'se) '(2 . 2) "se")
   (check-equal? (move-pos p 's ) '(1 . 2) "s" )
   (check-equal? (move-pos p 'sw) '(0 . 2) "sw")
   (check-equal? (move-pos p 'w ) '(0 . 1) "w" )
   (check-equal? (move-pos p 'nw) '(0 . 0) "nw")))

(define (move state action)
  (match-let* ([(cons 'move (cons actor-key direction)) action]
               [(hash* ['actors actors] ['level (hash* ['terrain terrain])]) state]
               [actor (hash-ref actors actor-key)]
               [pos (hash-ref actor 'pos)]
               [new-pos (move-pos pos direction)]
               [(cons (hash* ['def dest-terrain-def]) dest-actor)
                (object-at-pos new-pos terrain actors)])
    (if dest-actor
        (match dest-actor
          [(cons dest-actor-key dest-actor)
           (let-values ([(updated-actor updated-dest-actor) (melee actor dest-actor)])
             (cleanup-state (nested-hash-set
                             (nested-hash-set
                              state
                              'actors actor-key
                              updated-actor)
                             'actors dest-actor-key
                             updated-dest-actor)))])
        (if (equal? (hash-ref dest-terrain-def 'enter-cost) +inf.0)
            state
            (nested-hash-set state
                             'actors actor-key 'pos
                             new-pos)))))

(module+ test
  (define (make-ground pos)
    (hash 'pos pos
          'def (hash 'name "ground"
                     'glyph #\space
                     'enter-cost 0
                     'exit-cost 0)))
  (define state
    (hash 'actors (hash 'pc (hash 'pos p))
          'level (hash 'id 0
                       'terrain (map (lambda (y) (map (lambda (x) (make-ground `(,x . ,y)))
                                                      (range 3)))
                                     (range 3)))))

  (test-case
   "move"
   (check-equal? (nested-hash-ref (move state '(move . (pc . n ))) 'actors 'pc 'pos) '(1 . 0) "n" )
   (check-equal? (nested-hash-ref (move state '(move . (pc . ne))) 'actors 'pc 'pos) '(2 . 0) "ne")
   (check-equal? (nested-hash-ref (move state '(move . (pc . e ))) 'actors 'pc 'pos) '(2 . 1) "e" )
   (check-equal? (nested-hash-ref (move state '(move . (pc . se))) 'actors 'pc 'pos) '(2 . 2) "se")
   (check-equal? (nested-hash-ref (move state '(move . (pc . s ))) 'actors 'pc 'pos) '(1 . 2) "s" )
   (check-equal? (nested-hash-ref (move state '(move . (pc . sw))) 'actors 'pc 'pos) '(0 . 2) "sw")
   (check-equal? (nested-hash-ref (move state '(move . (pc . w ))) 'actors 'pc 'pos) '(0 . 1) "w" )
   (check-equal? (nested-hash-ref (move state '(move . (pc . nw))) 'actors 'pc 'pos) '(0 . 0) "nw")))
