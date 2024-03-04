#lang racket

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define (lat? l)
    (if (null? l)
     #t
     (and (atom? (car l)) (lat? (cdr l)))))

(lat? '(1 3 2 2  4 5))
