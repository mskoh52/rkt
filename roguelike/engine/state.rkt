#lang racket/base

(provide init-state)

(require
 racket/list
 "level.rkt")

(define (init-state level)
  (hash
   'actors (hash
            'pc (hash 'pos (Terrain-pos
                            (findf (lambda (t) (eq? (TerrainDef-name (Terrain-def t)) 'stair-up))
                                   (flatten (Level-terrain level)))))
            )
   'level level))
