#lang racket/base

(require "board.rkt")

(provide board-lift2
         board-equal?
         board-if
         board-if/strict
         board-and
         board-or
         board-not
         board-const
         board-compose
         board-pipe
         board-map
         board-seq)

;;; A combinator is a function: board -> X
;;; A factory is a function that returns a combinator.
;;;
;;; board-if, board-and, board-or, board-not  — control flow
;;; board-const, board-compose, board-pipe     — value plumbing
;;; board-map, board-seq                       — list / pairing

;;; ---------- Control flow ----------

(define (board-if pred-comb then-comb else-comb)
  ; Returns a combinator. pred-comb is applied to the board;
  ; the branch chosen also receives the original, unmodified board.
  (lambda (b)
    (if (pred-comb b)
        (then-comb b)
        (else-comb b))))

(define (board-if/strict pred-comb then-comb else-comb)
  (lambda (b)
    (define result (pred-comb b))
    (unless (boolean? result)
      (error 'board-if/strict "predicate returned non-boolean: ~a" result))
    (if result (then-comb b) (else-comb b))))

(define (board-and . combs)
  ; Short-circuits on first #f. Returns last truthy value or #f.
  (lambda (b)
    (let loop ([cs combs])
      (cond
        [(null? cs) #t]
        [else
         (define v ((car cs) b))
         (if (not v) #f (if (null? (cdr cs)) v (loop (cdr cs))))]))))

(define (board-or . combs)
  ; Short-circuits on first truthy value.
  (lambda (b)
    (let loop ([cs combs])
      (cond
        [(null? cs) #f]
        [else
         (define v ((car cs) b))
         (or v (loop (cdr cs)))]))))

(define (board-not comb)
  (lambda (b) (not (comb b))))

;;; ---------- Binary lifting ----------

(define (board-lift2 f comb1 comb2)
  ; Applies f to the results of two combinators on the same board.
  (lambda (b) (f (comb1 b) (comb2 b))))

(define (board-equal? comb1 comb2)
  (board-lift2 equal? comb1 comb2))

;;; ---------- Value plumbing ----------

(define (board-const v)
  ; Ignores the board, always returns v.
  (lambda (b) v))

(define (board-compose comb proc)
  ; Applies comb to board, feeds result into plain function proc.
  ; proc is NOT a combinator — it is a plain X -> Y function.
  ; #f is passed through: callers handle null-safety.
  (lambda (b) (proc (comb b))))

(define (board-pipe board-comb query-comb)
  ; board-comb must be board -> board.
  ; Transforms the board with board-comb, then applies query-comb to the new board.
  (lambda (b) (query-comb (board-comb b))))

;;; ---------- List / pairing ----------

(define (board-map list-comb f)
  ; Applies list-comb to board to get a list, then maps plain function f over it.
  (lambda (b) (map f (list-comb b))))

(define (board-seq . combs)
  ; Applies every combinator to the same board, returns list of results.
  (lambda (b) (map (lambda (c) (c b)) combs)))
