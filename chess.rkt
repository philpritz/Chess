#lang racket/base

(require "board.rkt"
         "combinators.rkt"
         "movegen.rkt")

(provide
 ; Re-export everything users need
 (all-from-out "board.rkt")
 (all-from-out "combinators.rkt")

 ; Square combinators
 coord

 ; Piece combinators (factories: take combinators, return combinators)
 piece-at
 square-empty?
 moves-from
 apply-move

 ; Board-value combinators (already combinators, not factories)
 side-to-move
 in-check?
 legal-moves

 ; Square combinator factories
 king-coord

 ; Plain piece predicates (use with board-compose)
 pawn? rook? knight? bishop? queen? king-piece? white? black?)

;;; ---------- coord ----------

; coord lifts an algebraic square symbol to a board -> square combinator.
; It ignores the board — the square is fixed at construction time.
; Use dynamic combinator factories like king-coord for computed squares.
(define (coord alg-sym)
  (board-const (alg->sq alg-sym)))

;;; ---------- Square combinator factories ----------

; king-coord: takes a color combinator (board -> 'white|'black),
; returns a square combinator (board -> square) for that king's position.
(define (king-coord color-comb)
  (lambda (b)
    (find-king b (color-comb b))))

;;; ---------- Chess combinators ----------

; piece-at: (board->square) -> (board -> piece|#f)
(define (piece-at sq-comb)
  (lambda (b)
    (define sq (sq-comb b))
    (and sq (board-ref b sq))))

; square-empty?: (board->square) -> (board -> bool)
(define (square-empty? sq-comb)
  (lambda (b)
    (not ((piece-at sq-comb) b))))

; side-to-move: board -> 'white|'black
(define side-to-move
  (lambda (b) (board-to-move b)))

; in-check?: board -> bool
(define in-check?
  (lambda (b) (king-attacked? b (board-to-move b))))

; legal-moves: board -> (listof move)
(define legal-moves
  (lambda (b) (generate-legal-moves b)))

; moves-from: (board->square) -> (board -> (listof move))
(define (moves-from sq-comb)
  (lambda (b)
    (define sq (sq-comb b))
    (filter (lambda (m) (equal? (move-from m) sq))
            (generate-legal-moves b))))

; apply-move: (board->move) -> (board -> board)
; The argument is a combinator that produces a move from the board.
(define (apply-move mv-comb)
  (lambda (b)
    (define m (mv-comb b))
    (and m (make-move b m))))

;;; ---------- Plain piece predicates ----------

(define (pawn?       p) (and (piece? p) (eq? (piece-type p) 'pawn)))
(define (rook?       p) (and (piece? p) (eq? (piece-type p) 'rook)))
(define (knight?     p) (and (piece? p) (eq? (piece-type p) 'knight)))
(define (bishop?     p) (and (piece? p) (eq? (piece-type p) 'bishop)))
(define (queen?      p) (and (piece? p) (eq? (piece-type p) 'queen)))
(define (king-piece? p) (and (piece? p) (eq? (piece-type p) 'king)))
(define (white?      p) (and (piece? p) (eq? (piece-color p) 'white)))
(define (black?      p) (and (piece? p) (eq? (piece-color p) 'black)))
