#lang racket

(require rackunit
         "../board.rkt")

(define start (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

;;; alg->sq / sq->alg
(check-equal? (alg->sq 'e4) (square 4 3))
(check-equal? (alg->sq 'a1) (square 0 0))
(check-equal? (alg->sq 'h8) (square 7 7))
(check-equal? (sq->alg (square 4 3)) 'e4)
(check-equal? (sq->alg (square 0 0)) 'a1)

;;; board-ref on starting position
(check-equal? (board-ref start (alg->sq 'e1)) (piece 'white 'king))
(check-equal? (board-ref start (alg->sq 'e8)) (piece 'black 'king))
(check-equal? (board-ref start (alg->sq 'd1)) (piece 'white 'queen))
(check-equal? (board-ref start (alg->sq 'a1)) (piece 'white 'rook))
(check-equal? (board-ref start (alg->sq 'g1)) (piece 'white 'knight))
(check-equal? (board-ref start (alg->sq 'a2)) (piece 'white 'pawn))
(check-false  (board-ref start (alg->sq 'e4)))
(check-false  (board-ref start (alg->sq 'e5)))

;;; board-set
(define b2 (board-set start (alg->sq 'e4) (piece 'white 'pawn)))
(check-equal? (board-ref b2 (alg->sq 'e4)) (piece 'white 'pawn))
(check-false  (board-ref start (alg->sq 'e4))) ; original unchanged

;;; board-to-move
(check-equal? (board-to-move start) 'white)

;;; castling rights
(check-true  (castling-rights-white-kingside  (board-castling start)))
(check-true  (castling-rights-white-queenside (board-castling start)))
(check-true  (castling-rights-black-kingside  (board-castling start)))
(check-true  (castling-rights-black-queenside (board-castling start)))

;;; en passant
(check-false (board-en-passant start))

;;; find-king
(check-equal? (find-king start 'white) (alg->sq 'e1))
(check-equal? (find-king start 'black) (alg->sq 'e8))

;;; opponent
(check-equal? (opponent 'white) 'black)
(check-equal? (opponent 'black) 'white)

;;; FEN with en passant
(define ep-board (fen->board "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"))
(check-equal? (board-en-passant ep-board) (alg->sq 'e3))
(check-equal? (board-to-move ep-board) 'black)

(displayln "test-board.rkt: all tests passed")
