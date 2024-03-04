#lang racket/base

(require
 racket/file
 racket/list
 math/array
 toml)

(provide
 load-level
 load-terrain-definitions
 object-at-pos)

;; (struct Level
;;   (id
;;    terrain))

;; (struct TerrainDef
;;   (name
;;    glyph
;;    enter-cost
;;    exit-cost))

;; (struct Terrain
;;   (pos
;;    def))

(define levels (make-hash))

(define (load-level filepath terrain-definitions)
  (let* ([f (open-input-file filepath)]
         [id (read-line f)]
         [y -1])
    id
    (hash
     'id id
     'terrain (for/list ([line (in-lines f)])
                (set! y (+ y 1))
                (map (lambda (x glyph)
                       (hash
                        'pos (cons x y)
                        'def (terrain-by-glyph glyph terrain-definitions)))
                     (range (string-length line))
                     (string->list line))))))

(define (load-terrain-definitions filepath)
  (define config (parse-toml (file->string filepath)))
  (hash-map
   config
   (lambda (k v)
     (let ([t (hash-ref config k)])
       (hash 'name k
             'glyph (car (string->list (hash-ref t 'glyph)))
             'enter-cost (hash-ref t 'enter-cost)
             'exit-cost (hash-ref t 'exit-cost))))))

(define (terrain-by-name terrain-name terrain-definitions)
  (findf (lambda (x) (eq? (hash-ref x 'name) terrain-name)) terrain-definitions))

(define (terrain-by-glyph glyph terrain-definitions)
  (findf (lambda (x) (eq? (hash-ref x 'glyph) glyph)) terrain-definitions))

(define (object-at-pos pos terrain actors)
  (cons (list-ref (list-ref terrain (cdr pos)) (car pos))
        (findf (lambda (actor) (equal? pos (hash-ref (cdr actor) 'pos))) (hash->list actors))))

(module+ test
  (require
   rackunit
   "state.rkt")

  (define terrain '((0 0 0 0 0 0 0)
                    (0 1 1 1 1 1 0)
                    (0 0 0 0 0 0 0)))

  (define actors (hash 'pc (hash 'glyph #\@ 'pos '(2 . 1))
                       'npc (hash 'glyph #\n 'pos '(4 . 1))))

  (test-case
   "object-at-pos"
   (check-equal? (object-at-pos '(0 . 0) terrain actors) '(0 . #f))
   (check-equal? (object-at-pos '(2 . 1) terrain actors) `(1  . (pc . ,(hash-ref actors 'pc))))
   (check-equal? (object-at-pos '(4 . 1) terrain actors) `(1  . (npc . ,(hash-ref actors 'npc))))))
