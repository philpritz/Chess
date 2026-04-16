#lang racket/base

(require racket/string
         racket/list
         racket/vector)

(provide (struct-out piece)
         (struct-out square)
         (struct-out move)
         (struct-out castling-rights)
         (struct-out board)
         board-ref
         board-set
         alg->sq
         sq->alg
         ->square
         find-king
         sq->idx
         idx->sq
         fen->board
         starting-board
         opponent)

;;; ---------- Piece ----------

(struct piece (color type) #:transparent)
; color: 'white | 'black
; type:  'pawn | 'rook | 'knight | 'bishop | 'queen | 'king

;;; ---------- Square ----------

(struct square (file rank) #:transparent)
; file: 0-7  (a=0 … h=7)
; rank: 0-7  (1st=0 … 8th=7)
; vector index = rank*8 + file

(define (alg->sq sym)
  (define s (symbol->string sym))
  (define file (- (char->integer (string-ref s 0)) (char->integer #\a)))
  (define rank (- (char->integer (string-ref s 1)) (char->integer #\1)))
  (square file rank))

(define (sq->alg sq)
  (string->symbol
   (string (integer->char (+ (char->integer #\a) (square-file sq)))
           (integer->char (+ (char->integer #\1) (square-rank sq))))))

(define (->square s)
  (cond
    [(square? s) s]
    [(symbol? s) (alg->sq s)]
    [else (error '->square "not a square designator: ~a" s)]))

(define (sq->idx sq) (+ (* (square-rank sq) 8) (square-file sq)))
(define (idx->sq i)  (square (remainder i 8) (quotient i 8)))

;;; ---------- Move ----------

(struct move (from to promotion) #:transparent)
; from, to: square structs
; promotion: #f | 'queen | 'rook | 'bishop | 'knight

;;; ---------- Castling rights ----------

(struct castling-rights
  (white-kingside white-queenside black-kingside black-queenside)
  #:transparent)

(define no-castling (castling-rights #f #f #f #f))
(define all-castling (castling-rights #t #t #t #t))

;;; ---------- Board ----------

(struct board
  (squares          ; vector[64]: piece or #f
   to-move          ; 'white | 'black
   castling         ; castling-rights
   en-passant       ; square or #f
   halfmove-clock   ; integer
   fullmove-number) ; integer
  #:transparent)

(define (board-ref b sq)
  (vector-ref (board-squares b) (sq->idx sq)))

(define (board-set b sq piece-or-false)
  (define new-vec (vector-copy (board-squares b)))
  (vector-set! new-vec (sq->idx sq) piece-or-false)
  (struct-copy board b [squares new-vec]))

;;; ---------- Helpers ----------

(define (opponent color)
  (if (eq? color 'white) 'black 'white))

(define (find-king b color)
  (define squares (board-squares b))
  (for/first ([i (in-range 64)]
              #:when (let ([p (vector-ref squares i)])
                       (and p
                            (eq? (piece-color p) color)
                            (eq? (piece-type p) 'king))))
    (idx->sq i)))

;;; ---------- FEN parser ----------

(define (fen->board fen-string)
  (define parts (string-split fen-string " "))
  (define ranks-str  (list-ref parts 0))
  (define to-move    (if (string=? (list-ref parts 1) "w") 'white 'black))
  (define castle-str (list-ref parts 2))
  (define ep-str     (list-ref parts 3))
  (define half-clock (if (> (length parts) 4) (string->number (list-ref parts 4)) 0))
  (define full-num   (if (> (length parts) 5) (string->number (list-ref parts 5)) 1))

  ; Parse piece placement (ranks from 8th to 1st in FEN)
  (define squares-vec (make-vector 64 #f))
  (for ([rank-str (in-list (reverse (string-split ranks-str "/")))]
        [rank-idx (in-range 8)])
    (define file-idx 0)
    (for ([ch (in-string rank-str)])
      (cond
        [(char-numeric? ch)
         (set! file-idx (+ file-idx (- (char->integer ch) (char->integer #\0))))]
        [else
         (define color (if (char-upper-case? ch) 'white 'black))
         (define type  (case (char-downcase ch)
                         [(#\p) 'pawn]  [(#\r) 'rook] [(#\n) 'knight]
                         [(#\b) 'bishop] [(#\q) 'queen] [(#\k) 'king]))
         (vector-set! squares-vec (+ (* rank-idx 8) file-idx) (piece color type))
         (set! file-idx (+ file-idx 1))])))

  ; Castling rights
  (define (has-char? s c) (regexp-match? (regexp-quote c) s))
  (define cr
    (castling-rights (has-char? castle-str "K")
                     (has-char? castle-str "Q")
                     (has-char? castle-str "k")
                     (has-char? castle-str "q")))

  ; En passant square
  (define ep (if (string=? ep-str "-") #f (alg->sq (string->symbol ep-str))))

  (board squares-vec to-move cr ep half-clock full-num))

(define starting-board
  (fen->board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
