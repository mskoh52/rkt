#lang racket/base

(require
 racket/file
 racket/list
 math/array
 toml)

(provide
 (struct-out Level)
 (struct-out Terrain)
 (struct-out TerrainDef)
 load-level
 load-terrain-definitions)

(struct Level
  (id
   terrain))

(struct TerrainDef
  (name
   glyph
   enter-cost
   exit-cost))

(struct Terrain
  (pos
   def))

(define levels (make-hash))

(define (load-level filepath terrain-definitions)
  (let* ([f (open-input-file filepath)]
         [id (read-line f)]
         [y -1])
    id
    (Level
     id
     (for/list ([line (in-lines f)])
       (set! y (+ y 1))
       (map (lambda (x glyph)
              (Terrain
               (cons x y)
               (terrain-by-glyph glyph terrain-definitions)))
            (range (string-length line))
            (string->list line))))))

(define (load-terrain-definitions filepath)
  (define config (parse-toml (file->string filepath)))
  (hash-map
   config
   (lambda (k v)
     (let ([t (hash-ref config k)])
       (TerrainDef k
                   (car (string->list (hash-ref t 'glyph)))
                   (hash-ref t 'enter-cost)
                   (hash-ref t 'exit-cost))))))

(define (terrain-by-name terrain-name terrain-definitions)
  (findf (lambda (x) (eq? (TerrainDef-name x) terrain-name)) terrain-definitions))

(define (terrain-by-glyph glyph terrain-definitions)
  (findf (lambda (x) (eq? (TerrainDef-glyph x) glyph)) terrain-definitions))
