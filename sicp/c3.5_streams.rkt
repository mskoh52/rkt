#lang sicp

(define (range start end)
  (define (_range i accum)
    (if (= i start)
        (cons i accum)
        (_range (- i 1) (cons i accum))))
  (_range (- end 1) '()))

(define (divisible? x y)
  (= (modulo x y) 0))

(define (prime? n)
  (define (iter i)
    (cond ((> i (sqrt n)) #t)
          ((or (divisible? n i) (divisible? n (+ i 2))) #f)
          (else (iter (+ i 1)))))
  (if (<= n 3)
      (> n 1)
      (if (or (divisible? n 2) (divisible? n 3))
          #f
          (iter 5))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; (define the-empty-stream '())
;; (define stream-null? null?)
(define-syntax delay-
  (syntax-rules ()
    ((delay- a)
     (memo-proc (lambda () a)))))

(define-syntax cons-stream-
  (syntax-rules ()
    ((cons-stream- a b)
     (cons a (delay- b)))))

(define (force- delayed-object) (delayed-object))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force- (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each- proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each- proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each- display-line s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream-
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter- pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream- (stream-car stream)
                       (stream-filter- pred (stream-cdr stream))))
        (else (stream-filter- pred (stream-cdr stream)))))


;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream-
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
