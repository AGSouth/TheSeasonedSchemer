;; #lang racket
#lang scheme

(require "../TheLittleSchemer/TheLittleSchemer.rkt")

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; Chapter 11 of The Seasoned Schemer
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) (quote ()))
      (else
        (cons (pick (car tup)
                    (cons (car tup) rev-pre))
              (scramble-b (cdr tup)
                     (cons (car tup) rev-pre)))))))

(define scramble
   (lambda (tup)
     (scramble-b tup (quote ()))))

 (define multirember
    (lambda (a lat)
      ((Y (lambda (mr)
            (lambda (lat)
              (cond
                 ((null? lat) (quote ()))
                 ((eq? a (car lat))
                    (mr (cdr lat)))
                  (else (cons (car lat)
                               (mr (cdr lat))))))))
              lat)))

;; from Simply Scheme trying to understand lambda
;; so that I can understand Y combinators

(define (same-arg-twice fn)
  (lambda (arg) (fn arg arg)))
