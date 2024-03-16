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
           nested-hash
           racket/pretty
           "engine/actions/dispatch.rkt")

  (check-equal? (nested-hash-ref state 'actors 'pc 'pos) '(5 . 3))

  ;; TODO It would be cool if we could provide a stream of actions (and/or inputs!) to
  ;; the loop function instead of needing to import `dispatch-action` and call it repeatedly.
  (check-equal? (nested-hash-ref ((dispatch-action '(move . (pc . n))) state) 'actors 'pc 'pos)
                '(5 . 2))

  (check-equal? (nested-hash-ref ((dispatch-action '(move . (pc . s))) state) 'actors 'pc 'pos)
                '(5 . 4))

  (test-case "move into wall"
    (check-equal?
     ((dispatch-action '(move . (pc . n))) ((dispatch-action '(move . (pc . n))) state))
     ((dispatch-action '(move . (pc . n)))
      ((dispatch-action '(move . (pc . n))) ((dispatch-action '(move . (pc . n))) state)))
     "moving three times is the same as moving two times"))

  (test-case "move into goblin"
    (let* ([three-move-state ((dispatch-action '(move . (pc . e)))
                              ((dispatch-action '(move . (pc . e)))
                               ((dispatch-action '(move . (pc . e))) state)))]
           [four-move-state ((dispatch-action '(move . (pc . e))) three-move-state)])
      (check-equal? (nested-hash-ref three-move-state 'actors 'pc 'pos) '(7 . 3))
      (check-false (hash-has-key? (hash-ref three-move-state 'actors) 'goblin))
      (check-equal? (nested-hash-ref four-move-state 'actors 'pc 'pos) '(8 . 3))
      (check-false (hash-has-key? (hash-ref four-move-state 'actors) 'goblin)))
    "moving three times doesn't change pos, but goblin is dead"))
