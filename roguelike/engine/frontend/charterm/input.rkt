#lang racket/base

(provide make-get-action)

(require racket/match
         "charterm.rkt")

(define (key->action keys)
  ;;; returns an action as defined in actions/dispatch.rkt
  (cond
    [(equal? ":exit" (list->string (reverse (filter char? keys)))) 'exit]
    [(or (null? keys) (member (car keys) '(#\: #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))) #f]
    [else
     (match keys
       ['(ctrl-q) 'exit]
       ['(ctrl-x)
        (begin
          (charterm-display " ")
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
