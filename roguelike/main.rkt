#lang racket/base

(require
 "engine/level.rkt"
 "engine/state.rkt"
 "engine/render/term.rkt")

(define level (load-level "level.txt" (load-terrain-definitions "terrain.toml")))
(define state (init-state level))

(module+ main
  (main state))

(module+ test
  (require
   rackunit
   nested-hash
   "engine/actions/dispatch.rkt")

  (check-equal? (nested-hash-ref state 'actors 'pc 'pos) '(5 . 3))

  ;; TODO It would be cool if we could provide a stream of actions (and/or inputs!) to
  ;; the loop function instead of needing to import `dispatch-action` and call it repeatedly.
  (check-equal?
   (nested-hash-ref ((dispatch-action '(move . (pc . n))) state)
                    'actors 'pc 'pos)
   '(5 . 2))

  (check-equal?
   (nested-hash-ref ((dispatch-action '(move . (pc . s))) state)
                    'actors 'pc 'pos)
   '(5 . 4)))
