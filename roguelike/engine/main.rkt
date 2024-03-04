#lang racket/base

(require
 racket/list
 racket/match
 "render/charterm.rkt"
 "level.rkt"
 "structs.rkt"
 "render/term.rkt")

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

(define (update-state state key)
  (match key
    [#\h (hash-set state 'player (move (hash-ref state 'player) 'w))]
    [#\j (hash-set state 'player (move (hash-ref state 'player) 's))]
    [#\k (hash-set state 'player (move (hash-ref state 'player) 'n))]
    [#\l (hash-set state 'player (move (hash-ref state 'player) 'e))]
    [#\y (hash-set state 'player (move (hash-ref state 'player) 'nw))]
    [#\u (hash-set state 'player (move (hash-ref state 'player) 'ne))]
    [#\n (hash-set state 'player (move (hash-ref state 'player) 'sw))]
    [#\m (hash-set state 'player (move (hash-ref state 'player) 'se))]
    [_ state]))

(define (loop state)
  (draw! state)
  (let ((key (charterm-read-key)))
    (if (eq? key #\q)
        (exit)
        (loop (update-state state key)))))

(define (init-state level)
  (hash
   'player (hash
            'pos (Terrain-pos
                  (findf (lambda (t) (eq? (TerrainDef-name (Terrain-def t)) 'stair-up))
                         (flatten (Level-terrain level)))))
   'level level))

(define level (load-level "level.txt" (load-terrain-definitions "terrain.toml")))

(with-charterm
 (loop (init-state level)))
