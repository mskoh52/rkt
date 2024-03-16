#lang racket/base

(require racket/match
         racket/pretty
         "charterm.rkt"
         "../loop.rkt")

(provide main)

(define (level-to-str level)
  (map (lambda (row) (list->string (map (lambda (t) (hash-ref (hash-ref t 'def) 'glyph)) row)))
       (hash-ref level 'terrain)))

(define (draw-level! level)
  (draw-lines! (level-to-str level)))

(define (draw-lines! lines)
  (for ([line lines])
    (charterm-display line)
    (charterm-newline)))

(define (draw-actor! actor)
  (let ([pos (hash-ref actor 'pos)])
    (charterm-cursor (+ 1 (car pos)) (+ 1 (cdr pos)))
    (charterm-display (hash-ref actor 'glyph))))

(define (draw-state! state)
  (charterm-cursor 1 32)
  (charterm-display (pretty-format state)))

(define input-pos '(1 . 30))

(define (draw! state)
  (charterm-cursor 1 1)
  (draw-level! (hash-ref state 'level))
  (for ([actor (hash-map (hash-ref state 'actors) (lambda (k v) v))])
    (draw-actor! actor))
  (charterm-cursor (car input-pos) (cdr input-pos)))

(define (key->action keys)
  ;;; returns an action as defined in actions/dispatch.rkt
  (cond
   [(equal? ":exit" (list->string (reverse (filter char? keys)))) 'exit]
   [(or (null? keys) (member (car keys) '(#\: #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))) #f]
   [else (match keys
           ['(ctrl-q) 'exit]
           ['(ctrl-x) (begin (charterm-display " ")
                             #f)]
           ['(ctrl-c ctrl-x) 'exit]
           [(cons #\k _) '(move . (pc . n))]
           [(cons #\u _) '(move . (pc . ne))]
           [(cons #\l _) '(move . (pc . e))]
           [(cons #\m _) '(move . (pc . se))]
           [(cons #\j _) '(move . (pc . s))]
           [(cons #\n _) '(move . (pc . sw))]
           [(cons #\h _) '(move . (pc . w))]
           [(cons #\y _) '(move . (pc . nw))]
           [_ '()])]))

(define (make-get-action row)
  (define (get-action)
    (define (loop keys cmd-mode?)
      (let* ([key (charterm-read-key)]
             [to-display (if (char? key) (string key) (symbol->string key))]
             [keys (cons key keys)])
        (charterm-display to-display)
        (if (or (and cmd-mode? (not (eq? key 'return))) (not (key->action keys)))
            (loop keys (or cmd-mode? (eq? key #\:)))
            (key->action keys))))
    (charterm-cursor 1 row)
    (charterm-clear-line-right)
    (loop '() #f))
  get-action)

(define (main state)
  (set! input-pos `(1 . ,(+ 2 (length (hash-ref (hash-ref state 'level) 'terrain)))))
  (with-charterm
   (charterm-clear-screen)
   ((make-loop draw! (make-get-action (+ 2 (length (hash-ref (hash-ref state 'level) 'terrain)))))
    state)))
