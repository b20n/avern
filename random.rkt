
#lang racket

(define (lets x)
  (let ([foo (lambda (j) (+ x j))])
    (foo 45)))

(define (lambdas x)
  ((lambda (i) (i 45))
   (lambda (j) (+ x j))))

(define (lifts x)
  ((lambda (i) (i 45))
;; Note: this is crappy
   ((lambda (k) (lambda (j) (+ k j))) x)))
;; This is right, but racket doesn't do partial application by default
;;   ((lambda (j k) (+ k j)) x)))

(define (cps cont x)
  ((lambda (cont i) (cont (i 45)))
   ((lambda (cont k) (lambda (j) (cont (+ k j)))) cont x)))

(cps write 3)
;;(write (lifts 3))

