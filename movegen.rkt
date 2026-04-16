#lang racket/base

(require racket/list
         racket/vector
         "board.rkt"
         "combinators.rkt")

(provide generate-legal-moves
         generate-pseudo-legal-moves
         king-attacked?
         attacked-by?
         make-move)

(define (piece-at-sq b sq)
  (and (sq-valid-sq? sq) (board-ref b sq)))

(define (empty-sq? b sq)
  (not (piece-at-sq b sq)))

(define (enemy? b sq color)
  (define p (piece-at-sq b sq))
  (and p (not (eq? (piece-color p) color))))

(define (friendly? b sq color)
  (define p (piece-at-sq b sq))
  (and p (eq? (piece-color p) color)))

;;; ---------- Sliding rays ----------

(define (ray-moves b from color dirs)
  (for*/list ([d dirs]
              [step (in-list (let loop ([f (square-file from)]
                                        [r (square-rank from)]
                                        [acc '()])
                               (define nf (+ f (car d)))
                               (define nr (+ r (cdr d)))
                               (cond
                                 [(not (sq-valid? nf nr)) (reverse acc)]
                                 [(friendly? b (square nf nr) color) (reverse acc)]
                                 [(enemy? b (square nf nr) color)
                                  (reverse (cons (square nf nr) acc))]
                                 [else (loop nf nr (cons (square nf nr) acc))])))])
    (move from step #f)))

; Non-recursive version of ray generation
(define (sliding-moves b from color dirs)
  (define results '())
  (for ([d dirs])
    (let loop ([sq (sq+ from (car d) (cdr d))])
      (when (sq-valid-sq? sq)
        (cond
          [(friendly? b sq color) (void)] ; blocked
          [(enemy? b sq color)
           (set! results (cons (move from sq #f) results))]
          [else
           (set! results (cons (move from sq #f) results))
           (loop (sq+ sq (car d) (cdr d)))]))))
  results)

;;; ---------- Per-piece move generators ----------

(define (pawn-moves b from color)
  (define dir (if (eq? color 'white) 1 -1))
  (define start-rank (if (eq? color 'white) 1 6))
  (define promo-rank (if (eq? color 'white) 7 0))
  (define moves '())

  ; Single push
  (define one (sq+ from 0 dir))
  (when (and (sq-valid-sq? one) (empty-sq? b one))
    (if (= (square-rank one) promo-rank)
        (for ([pt '(queen rook bishop knight)])
          (set! moves (cons (move from one pt) moves)))
        (set! moves (cons (move from one #f) moves)))
    ; Double push from start rank
    (when (= (square-rank from) start-rank)
      (define two (sq+ from 0 (* 2 dir)))
      (when (and (sq-valid-sq? two) (empty-sq? b two))
        (set! moves (cons (move from two #f) moves)))))

  ; Captures
  (for ([df '(-1 1)])
    (define cap-sq (sq+ from df dir))
    (when (sq-valid-sq? cap-sq)
      (define is-ep (and (board-en-passant b)
                         (equal? cap-sq (board-en-passant b))))
      (when (or (enemy? b cap-sq color) is-ep)
        (if (= (square-rank cap-sq) promo-rank)
            (for ([pt '(queen rook bishop knight)])
              (set! moves (cons (move from cap-sq pt) moves)))
            (set! moves (cons (move from cap-sq #f) moves))))))

  moves)

(define (knight-moves b from color)
  (filter-map
   (lambda (d)
     (define sq (sq+ from (car d) (cdr d)))
     (and (sq-valid-sq? sq)
          (not (friendly? b sq color))
          (move from sq #f)))
   '((-2 . -1) (-2 . 1) (-1 . -2) (-1 . 2)
     (1 . -2) (1 . 2) (2 . -1) (2 . 1))))

(define bishop-dirs '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1)))
(define rook-dirs   '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
(define queen-dirs  (append bishop-dirs rook-dirs))

(define (bishop-moves b from color) (sliding-moves b from color bishop-dirs))
(define (rook-moves   b from color) (sliding-moves b from color rook-dirs))
(define (queen-moves  b from color) (sliding-moves b from color queen-dirs))

(define (king-normal-moves b from color)
  (filter-map
   (lambda (d)
     (define sq (sq+ from (car d) (cdr d)))
     (and (sq-valid-sq? sq)
          (not (friendly? b sq color))
          (move from sq #f)))
   (for*/list ([df '(-1 0 1)] [dr '(-1 0 1)]
               #:when (not (and (= df 0) (= dr 0))))
     (cons df dr))))

;;; ---------- Castling ----------

(define (castling-moves b color)
  ; We only generate castling moves here; legality (not passing through check)
  ; is enforced in generate-legal-moves via make-move + king-attacked?.
  (define cr (board-castling b))
  (define rank (if (eq? color 'white) 0 7))
  (define king-sq (square 4 rank))
  (define moves '())

  (when (equal? (board-ref b king-sq) (piece color 'king))
    ; Kingside
    (define ks? (if (eq? color 'white)
                    (castling-rights-white-kingside cr)
                    (castling-rights-black-kingside cr)))
    (when ks?
      (when (and (not (board-ref b (square 5 rank)))
                 (not (board-ref b (square 6 rank))))
        (set! moves (cons (move king-sq (square 6 rank) #f) moves))))

    ; Queenside
    (define qs? (if (eq? color 'white)
                    (castling-rights-white-queenside cr)
                    (castling-rights-black-queenside cr)))
    (when qs?
      (when (and (not (board-ref b (square 3 rank)))
                 (not (board-ref b (square 2 rank)))
                 (not (board-ref b (square 1 rank))))
        (set! moves (cons (move king-sq (square 2 rank) #f) moves)))))

  moves)

;;; ---------- Pseudo-legal move generation ----------

(define (moves-for-piece b sq p)
  (define color (piece-color p))
  (case (piece-type p)
    [(pawn)   (pawn-moves   b sq color)]
    [(knight) (knight-moves b sq color)]
    [(bishop) (bishop-moves b sq color)]
    [(rook)   (rook-moves   b sq color)]
    [(queen)  (queen-moves  b sq color)]
    [(king)   (append (king-normal-moves b sq color)
                      (castling-moves b color))]))

(define (generate-pseudo-legal-moves b)
  (define color (board-to-move b))
  (define squares (board-squares b))
  (for*/list ([i (in-range 64)]
              #:when (let ([p (vector-ref squares i)])
                       (and p (eq? (piece-color p) color)))
              [m (in-list (moves-for-piece b (idx->sq i)
                                          (vector-ref squares i)))])
    m))

;;; ---------- Attack detection (combinator-based) ----------

; ray-first-piece: sq-comb x dir -> (board -> piece|#f)
; Slides from sq in dir, returning the first piece hit or #f.
; This is the only primitive with a loop — everything above composes it.
(define (ray-first-piece sq-comb dir)
  (lambda (b)
    (define start (sq-comb b))
    (and start
         (let loop ([s (sq+ start (car dir) (cdr dir))])
           (and (sq-valid-sq? s)
                (or (board-ref b s)
                    (loop (sq+ s (car dir) (cdr dir)))))))))

; piece-matches?: p-comb x color-comb x piece-pred -> (board -> bool)
; Checks that p-comb yields a piece of the right color and type.
(define (piece-matches? p-comb by-color-comb piece-pred)
  (board-and
    p-comb
    (board-equal? (board-compose p-comb piece-color) by-color-comb)
    (board-compose p-comb piece-pred)))

; any-attacker?: (listof p-comb) x color-comb x piece-pred -> (board -> bool)
(define (any-attacker? piece-combs by-color-comb piece-pred)
  (apply board-or
    (map (lambda (pc) (piece-matches? pc by-color-comb piece-pred))
         piece-combs)))

; jump-candidates: sq-comb x deltas -> (listof p-comb)
(define (jump-candidates sq-comb deltas)
  (map (lambda (d) (piece-at (sq-offset sq-comb d))) deltas))

; slider-candidates: sq-comb x dirs -> (listof p-comb)
(define (slider-candidates sq-comb dirs)
  (map (lambda (d) (ray-first-piece sq-comb d)) dirs))

(define knight-deltas '((-2 . -1) (-2 . 1) (-1 . -2) (-1 . 2)
                         (1 . -2) (1 . 2) (2 . -1) (2 . 1)))
(define king-deltas
  (for*/list ([df '(-1 0 1)] [dr '(-1 0 1)]
              #:when (not (and (= df 0) (= dr 0))))
    (cons df dr)))

(define (bishop-or-queen? p) (and p (or (bishop? p) (queen? p))))
(define (rook-or-queen?   p) (and p (or (rook? p)   (queen? p))))

; pawn-attacks-sq?: sq-comb x color-comb -> (board -> bool)
; Pawn deltas are color-dependent so computed inside the lambda.
(define (pawn-attacks-sq? sq-comb by-color-comb)
  (lambda (b)
    (define by-color (by-color-comb b))
    (define dir (if (eq? by-color 'white) -1 1))
    ((any-attacker? (jump-candidates sq-comb (list (cons -1 dir) (cons 1 dir)))
                    by-color-comb pawn?)
     b)))

; attacked-by?: sq-comb x color-comb -> (board -> bool)
(define (attacked-by? sq-comb by-color-comb)
  (board-or
    (pawn-attacks-sq?   sq-comb by-color-comb)
    (any-attacker? (jump-candidates   sq-comb knight-deltas) by-color-comb knight?)
    (any-attacker? (slider-candidates sq-comb bishop-dirs)   by-color-comb bishop-or-queen?)
    (any-attacker? (slider-candidates sq-comb rook-dirs)     by-color-comb rook-or-queen?)
    (any-attacker? (jump-candidates   sq-comb king-deltas)   by-color-comb king-piece?)))

(define (king-attacked? b color)
  (define ksq (find-king b color))
  (and ksq ((attacked-by? (board-const ksq) (board-const (opponent color))) b)))

;;; ---------- make-move ----------

(define (make-move b m)
  (define from (move-from m))
  (define to   (move-to   m))
  (define promo (move-promotion m))
  (define mover (board-ref b from))
  (define color (piece-color mover))
  (define new-vec (vector-copy (board-squares b)))

  ; Move the piece
  (define moved-piece
    (if promo (piece color promo) mover))
  (vector-set! new-vec (sq->idx to)   moved-piece)
  (vector-set! new-vec (sq->idx from) #f)

  ; En passant capture: remove the captured pawn
  (define new-ep #f)
  (when (and (eq? (piece-type mover) 'pawn)
             (board-en-passant b)
             (equal? to (board-en-passant b)))
    (define captured-rank (square-rank from))
    (vector-set! new-vec (sq->idx (square (square-file to) captured-rank)) #f))

  ; Set new en passant square for double pawn push
  (when (and (eq? (piece-type mover) 'pawn)
             (= (abs (- (square-rank to) (square-rank from))) 2))
    (set! new-ep (square (square-file from)
                         (quotient (+ (square-rank from) (square-rank to)) 2))))

  ; Castling: move the rook
  (define rank (square-rank from))
  (when (and (eq? (piece-type mover) 'king)
             (= (abs (- (square-file to) (square-file from))) 2))
    (cond
      [(= (square-file to) 6) ; kingside
       (vector-set! new-vec (sq->idx (square 5 rank)) (piece color 'rook))
       (vector-set! new-vec (sq->idx (square 7 rank)) #f)]
      [(= (square-file to) 2) ; queenside
       (vector-set! new-vec (sq->idx (square 3 rank)) (piece color 'rook))
       (vector-set! new-vec (sq->idx (square 0 rank)) #f)]))

  ; Update castling rights
  (define old-cr (board-castling b))
  (define new-cr
    (castling-rights
     (and (castling-rights-white-kingside  old-cr)
          (not (and (eq? color 'white) (eq? (piece-type mover) 'king)))
          (not (equal? from (square 7 0)))
          (not (equal? to   (square 7 0))))
     (and (castling-rights-white-queenside old-cr)
          (not (and (eq? color 'white) (eq? (piece-type mover) 'king)))
          (not (equal? from (square 0 0)))
          (not (equal? to   (square 0 0))))
     (and (castling-rights-black-kingside  old-cr)
          (not (and (eq? color 'black) (eq? (piece-type mover) 'king)))
          (not (equal? from (square 7 7)))
          (not (equal? to   (square 7 7))))
     (and (castling-rights-black-queenside old-cr)
          (not (and (eq? color 'black) (eq? (piece-type mover) 'king)))
          (not (equal? from (square 0 7)))
          (not (equal? to   (square 0 7))))))

  ; Halfmove clock
  (define captured (board-ref b to))
  (define new-half
    (if (or (eq? (piece-type mover) 'pawn) captured)
        0
        (+ (board-halfmove-clock b) 1)))

  (board new-vec
         (opponent color)
         new-cr
         new-ep
         new-half
         (+ (board-fullmove-number b)
            (if (eq? color 'black) 1 0))))

;;; ---------- Legal move generation ----------

(define (castling-through-check? b m color)
  (define from (move-from m))
  (define to   (move-to   m))
  (and (eq? (piece-type (board-ref b from)) 'king)
       (= (abs (- (square-file to) (square-file from))) 2)
       (let* ([step (if (> (square-file to) (square-file from)) 1 -1)]
              [pass (square (+ (square-file from) step) (square-rank from))]
              [opp  (board-const (opponent color))])
         (or ((attacked-by? (board-const from) opp) b)
             ((attacked-by? (board-const pass) opp) b)))))

(define (generate-legal-moves b)
  (define color (board-to-move b))
  (filter
   (lambda (m)
     (and (not (castling-through-check? b m color))
          (not (king-attacked? (make-move b m) color))))
   (generate-pseudo-legal-moves b)))
