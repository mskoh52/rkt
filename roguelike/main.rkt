#lang racket/base

(require "engine/level.rkt"
         "engine/state.rkt"
         "engine/render/term.rkt")

(define level (load-level "level.txt" (load-terrain-definitions "terrain.toml")))
(define state (init-state level))

(module+ main
  (main state))

(module+ test
  (require rackunit
           racket/list
           racket/pretty
           nested-hash
           "engine/actions/dispatch.rkt")

  (check-equal? (nested-hash-ref state 'actors 'pc 'pos) '(5 . 3))

  (check-equal? (nested-hash-ref ((dispatch-action '(move . (pc . n))) state) 'actors 'pc 'pos)
                '(5 . 2))

  (check-equal? (nested-hash-ref ((dispatch-action '(move . (pc . s))) state) 'actors 'pc 'pos)
                '(5 . 4))

  (test-case "move into wall"
    (check-equal?
     (foldl (lambda (a s) ((dispatch-action a) s)) state (make-list 2 '(move . (pc . n))))
     (foldl (lambda (a s) ((dispatch-action a) s)) state (make-list 3 '(move . (pc . n))))
     "moving three times is the same as moving two times"))

  (test-case "move into goblin"
    (let* ([three-move-state
            (foldl (lambda (a s) ((dispatch-action a) s)) state (make-list 3 '(move . (pc . e))))]
           [four-move-state
            (foldl (lambda (a s) ((dispatch-action a) s)) state (make-list 4 '(move . (pc . e))))])
      (check-equal? (nested-hash-ref three-move-state 'actors 'pc 'pos) '(7 . 3))
      (check-false (hash-has-key? (hash-ref three-move-state 'actors) 'goblin))
      (check-equal? (nested-hash-ref four-move-state 'actors 'pc 'pos) '(8 . 3))
      (check-false (hash-has-key? (hash-ref four-move-state 'actors) 'goblin)))
    "moving three times doesn't change pos, but goblin is dead"))
