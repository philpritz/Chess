#lang racket/base

(require "board.rkt"
         "combinators.rkt"
         "movegen.rkt")

(provide
 (all-from-out "board.rkt")
 (all-from-out "combinators.rkt")
 coord
 square-empty?
 moves-from
 apply-move
 side-to-move
 opp-side
 in-check?
 legal-moves
 king-coord
 attacked-by?)

;;; ---------- coord ----------

(define (coord alg-sym)
  (board-const (alg->sq alg-sym)))

;;; ---------- Square combinator factories ----------

(define (king-coord color-comb)
  (lambda (b) (find-king b (color-comb b))))

;;; ---------- Chess combinators ----------

(define (square-empty? sq-comb)
  (board-not (piece-at sq-comb)))

(define side-to-move board-to-move)

(define opp-side
  (board-compose side-to-move opponent))

(define in-check?
  (attacked-by? (king-coord side-to-move) opp-side))

(define legal-moves generate-legal-moves)

(define (moves-from sq-comb)
  (lambda (b)
    (define sq (sq-comb b))
    (filter (lambda (m) (equal? (move-from m) sq))
            (generate-legal-moves b))))

(define (apply-move mv-comb)
  (lambda (b)
    (define m (mv-comb b))
    (and m (make-move b m))))
