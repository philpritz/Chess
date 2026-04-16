#lang racket

(require "chess.rkt")

;;; ============================================================
;;; Combinator Chess Language — Example Programs
;;; ============================================================
;;;
;;; Every combinator has type: board -> X
;;; Factories return combinators: they take combinators as args.
;;;
;;; Core vocabulary:
;;;   (coord 'e4)                        ; board -> square  (static)
;;;   (piece-at sq-comb)                 ; board -> piece|#f
;;;   (square-empty? sq-comb)            ; board -> bool
;;;   (moves-from sq-comb)               ; board -> (listof move)
;;;   (apply-move mv-comb)               ; board -> board
;;;   side-to-move                       ; board -> color
;;;   in-check?                          ; board -> bool
;;;   legal-moves                        ; board -> (listof move)
;;;   (king-coord color-comb)            ; board -> square
;;;   (board-if pred then else)          ; board -> X
;;;   (board-compose comb plain-fn)      ; board -> Y
;;;   (board-const v)                    ; board -> v

;;; ---- Example 1: Is there a white pawn on e4? ----

(define white-pawn-on-e4?
  (board-if (piece-at (coord 'e4))
            (board-compose (piece-at (coord 'e4))
                           (lambda (p) (and (white? p) (pawn? p))))
            (board-const #f)))

;;; ---- Example 2: Escape-check moves ----
;;; Returns legal moves only when the side to move is in check.

(define escape-check-moves
  (board-if in-check?
            legal-moves
            (board-const '())))

;;; ---- Example 3: Does the moving side have any legal moves? ----
;;; pair? is truthy on non-empty lists, #f on '().

(define has-legal-moves?
  (board-compose legal-moves pair?))

;;; ---- Example 4: Apply the first legal move ----

(define apply-first-legal-move
  (board-if has-legal-moves?
            (apply-move (board-compose legal-moves car))
            (board-const #f)))

;;; ---- Example 5: Is the king on its starting square? ----
;;; king-coord uses the board to find where the king actually is.

(define white-king-on-e1?
  (board-compose (king-coord (board-const 'white))
                 (lambda (sq) (equal? sq (alg->sq 'e1)))))

;;; ---- Example 6: Piece on the king's square ----
;;; king-coord is a dynamic square combinator — composes with piece-at.

(define piece-on-king-square
  (piece-at (king-coord side-to-move)))

;;; ---- Example 7: A simple material counter ----

(define piece-value
  (lambda (p)
    (if (not p) 0
        (case (piece-type p)
          [(pawn)   1] [(knight) 3] [(bishop) 3]
          [(rook)   5] [(queen)  9] [(king)   0]
          [else 0]))))

(define (signed-value p)
  (if (not p) 0
      (* (piece-value p) (if (eq? (piece-color p) 'white) 1 -1))))

(define material-balance
  (lambda (b)
    (for/sum ([i (in-range 64)])
      (signed-value (vector-ref (board-squares b) i)))))

;;; ---- Example 8: Stalemate or checkmate detector ----

(define game-over?
  (board-not has-legal-moves?))

(define checkmate?
  (board-and in-check? game-over?))

(define stalemate?
  (board-and (board-not in-check?) game-over?))

;;; ---- Demo ----

(define (display-result label comb board)
  (printf "~a: ~a\n" label (comb board)))

(define (run-demo)
  (define start (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
  (define e4-board (fen->board "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"))
  ; Scholar's mate — white is checkmated
  (define checkmate-board (fen->board "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"))

  (displayln "=== Starting position ===")
  (display-result "white-pawn-on-e4?"    white-pawn-on-e4?    start)
  (display-result "has-legal-moves?"     has-legal-moves?     start)
  (display-result "in-check?"            in-check?            start)
  (display-result "material-balance"     material-balance     start)
  (display-result "checkmate?"           checkmate?           start)
  (display-result "stalemate?"           stalemate?           start)
  (display-result "legal-move-count"
                  (board-compose legal-moves length)
                  start)

  (displayln "\n=== After 1.e4 ===")
  (display-result "white-pawn-on-e4?"    white-pawn-on-e4?    e4-board)
  (display-result "side-to-move"         side-to-move         e4-board)

  (displayln "\n=== Scholar's mate position ===")
  (display-result "in-check?"            in-check?            checkmate-board)
  (display-result "checkmate?"           checkmate?           checkmate-board)
  (display-result "escape-check-moves"
                  (board-compose escape-check-moves length)
                  checkmate-board))

(run-demo)
