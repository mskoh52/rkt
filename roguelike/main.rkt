#lang racket/base

(require
 racket/list
 racket/match
 "engine/input.rkt"
 "engine/level.rkt"
 "engine/pos.rkt"
 "engine/render/term.rkt")


(define (init-state level)
  (hash
   'player (hash
            'pos (Terrain-pos
                  (findf (lambda (t) (eq? (TerrainDef-name (Terrain-def t)) 'stair-up))
                         (flatten (Level-terrain level)))))
   'level level))

(define level (load-level "level.txt" (load-terrain-definitions "terrain.toml")))

(main (init-state level))
