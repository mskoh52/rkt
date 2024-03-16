#lang racket/base

(provide init-state
         cleanup-state)

(require racket/list
         racket/match)

(define (init-state level)
  (hash 'actors
        (hash 'pc
              (hash 'type
                    "pc"
                    'glyph
                    #\@
                    'pos
                    (hash-ref (findf (lambda (t) (eq? (hash-ref (hash-ref t 'def) 'name) 'stair-up))
                                     (flatten (hash-ref level 'terrain)))
                              'pos)
                    'hp
                    100
                    'def
                    100
                    'weapon
                    '("sword" 10 100))
              'goblin
              (hash 'type "goblin" 'glyph #\g 'pos '(8 . 3) 'hp 5 'def 5 'weapon '("club" 5 120)))
        'level
        level))

(define (cleanup-state state)
  (define (remove-dead actors)
    (make-immutable-hash (filter (lambda (kv) (> (hash-ref (cdr kv) 'hp) 0)) (hash->list actors))))
  (hash 'actors (remove-dead (hash-ref state 'actors)) 'level (hash-ref state 'level)))
