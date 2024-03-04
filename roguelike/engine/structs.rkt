#lang racket/base

(provide (struct-out Pos)
         (struct-out Character)
         (struct-out State))

(struct Pos (x y) #:transparent)

(struct Character
  (pos))

(struct State
  (player
   level))

(struct Terrain
  (pos
   glyph
   data))
