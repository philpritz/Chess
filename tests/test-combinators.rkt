#lang racket

(require rackunit
         "../board.rkt"
         "../combinators.rkt")

(define start (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

;;; board-const
(check-equal? ((board-const 42) start) 42)
(check-equal? ((board-const 'hello) start) 'hello)
(check-false  ((board-const #f) start))

;;; board-if — then branch
(check-equal? ((board-if (board-const #t)
                         (board-const 'yes)
                         (board-const 'no))
               start)
              'yes)

;;; board-if — else branch
(check-equal? ((board-if (board-const #f)
                         (board-const 'yes)
                         (board-const 'no))
               start)
              'no)

;;; board-if — truthy non-boolean pred (a struct is truthy)
(check-equal? ((board-if (board-const (piece 'white 'pawn))
                         (board-const 'piece-found)
                         (board-const 'empty))
               start)
              'piece-found)

;;; board-if — both branches receive original board
(define side-comb (lambda (b) (board-to-move b)))
(check-equal? ((board-if (board-const #t) side-comb side-comb) start) 'white)

;;; board-if/strict — error on non-boolean
(check-exn exn:fail?
           (lambda ()
             ((board-if/strict (board-const 42)
                               (board-const 'yes)
                               (board-const 'no))
              start)))

;;; board-not
(check-true  ((board-not (board-const #f)) start))
(check-false ((board-not (board-const #t)) start))

;;; board-and
(check-true  ((board-and (board-const #t) (board-const #t)) start))
(check-false ((board-and (board-const #t) (board-const #f)) start))
(check-false ((board-and (board-const #f) (board-const #t)) start))
(check-equal? ((board-and (board-const 'a) (board-const 'b)) start) 'b)

;;; board-or
(check-true  ((board-or (board-const #f) (board-const #t)) start))
(check-false ((board-or (board-const #f) (board-const #f)) start))
(check-equal? ((board-or (board-const #f) (board-const 'x)) start) 'x)

;;; board-compose — threads output into plain function
(check-equal? ((board-compose (board-const 5) (lambda (x) (* x 2))) start) 10)
(check-false  ((board-compose (board-const #f) (lambda (x) x)) start))

;;; board-pipe — transforms board first
(define identity-board-comb (lambda (b) b))
(check-equal? ((board-pipe identity-board-comb (board-const 99)) start) 99)

;;; board-seq
(define results ((board-seq (board-const 1) (board-const 2) (board-const 3)) start))
(check-equal? results '(1 2 3))

;;; board-map — maps over list result
(check-equal? ((board-map (board-const '(1 2 3)) (lambda (x) (* x 10))) start)
              '(10 20 30))

(displayln "test-combinators.rkt: all tests passed")
