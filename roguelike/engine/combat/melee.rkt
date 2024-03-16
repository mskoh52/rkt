#lang racket/base

(provide melee)

(require racket/match)

(define (melee src dst)
  (match-let ([(hash* ['hp src-hp] ['def src-def] ['weapon src-weapon]) src]
              [(hash* ['hp dst-hp] ['def dst-def] ['weapon dst-weapon]) dst])
    (values src (hash-set* dst 'hp (- dst-hp (max 0 (- (cadr src-weapon) dst-def)))))))
