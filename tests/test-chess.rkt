#lang racket

(require rackunit
         "../chess.rkt")

(define start (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
(define e4-board (fen->board "8/8/8/8/4P3/8/8/8 w - - 0 1"))
; Scholar's mate — white king in check from Qh4
(define check-board (fen->board "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"))
; Fool's mate — black checkmates white
(define checkmate-board (fen->board "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"))

;;; coord
(check-equal? ((coord 'e4) start) (alg->sq 'e4))
(check-equal? ((coord 'a1) start) (alg->sq 'a1))

;;; piece-at
(check-equal? ((piece-at (coord 'e1)) start) (piece 'white 'king))
(check-equal? ((piece-at (coord 'e8)) start) (piece 'black 'king))
(check-false  ((piece-at (coord 'e4)) start))
(check-equal? ((piece-at (coord 'e4)) e4-board) (piece 'white 'pawn))

;;; square-empty?
(check-false ((square-empty? (coord 'e1)) start))
(check-true  ((square-empty? (coord 'e4)) start))
(check-false ((square-empty? (coord 'e4)) e4-board))

;;; board-compose with piece predicates
(check-true  ((board-compose (piece-at (coord 'e4)) pawn?) e4-board))
(check-false ((board-compose (piece-at (coord 'e4)) pawn?) start))  ; #f → pawn? #f → #f

;;; side-to-move
(check-equal? (side-to-move start) 'white)
(define after-e4 (fen->board "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"))
(check-equal? (side-to-move after-e4) 'black)

;;; in-check?
(check-false (in-check? start))
(check-true  (in-check? check-board))

;;; legal-moves count
(define start-move-count (length (legal-moves start)))
(check-equal? start-move-count 20) ; 16 pawn + 4 knight moves

;;; moves-from
(define e2-moves (length ((moves-from (coord 'e2)) start)))
(check-equal? e2-moves 2) ; e3 and e4

;;; king-coord
(check-equal? ((king-coord (board-const 'white)) start) (alg->sq 'e1))
(check-equal? ((king-coord (board-const 'black)) start) (alg->sq 'e8))
(check-equal? ((king-coord side-to-move) start) (alg->sq 'e1))

;;; piece-at composed with king-coord
(check-equal? ((piece-at (king-coord side-to-move)) start)
              (piece 'white 'king))

;;; apply-move
(define first-move-board
  ((apply-move (board-compose legal-moves car)) start))
(check-equal? (board-to-move first-move-board) 'black)

;;; piece predicates
(check-true  (pawn?       (piece 'white 'pawn)))
(check-false (pawn?       (piece 'white 'rook)))
(check-true  (rook?       (piece 'black 'rook)))
(check-true  (knight?     (piece 'white 'knight)))
(check-true  (bishop?     (piece 'black 'bishop)))
(check-true  (queen?      (piece 'white 'queen)))
(check-true  (king-piece? (piece 'black 'king)))
(check-true  (white?      (piece 'white 'pawn)))
(check-false (white?      (piece 'black 'pawn)))
(check-true  (black?      (piece 'black 'pawn)))
(check-false (pawn? #f))
(check-false (white? #f))

;;; check-board is checkmate: in-check? true, no legal moves
(check-true  (in-check? check-board))
(check-equal? (length (legal-moves check-board)) 0)
(define escapes ((board-if in-check? legal-moves (board-const '())) check-board))
(check-equal? escapes '())

;;; stalemate check (synthetic: king alone, no moves)
(define stalemate-board (fen->board "k7/8/1Q6/8/8/8/8/7K b - - 0 1"))
(check-false (in-check? stalemate-board))
(check-equal? (length (legal-moves stalemate-board)) 0)

(displayln "test-chess.rkt: all tests passed")
