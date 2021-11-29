#lang racket

(require "../TheLittleSchemer/TheLittleSchemer.rkt")
# Chapter 11 of The Seasoned Schemer
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

    