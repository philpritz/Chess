# Combinator Chess

A chess domain language in Racket where **everything is a combinator** — a function `board -> X`. Combinators take combinators as arguments. There are no raw values passed around; static values are lifted with `board-const` or `coord`.

## Core Idea

A *combinator* is any function `board -> X`. A *factory* is a function that takes combinators and returns a new combinator. The central control-flow primitive is `board-if`:

```racket
(board-if pred-comb then-comb else-comb)
```

All three arguments are combinators applied to the **same** board. The predicate's result gates which branch runs — neither branch sees the predicate's output, just the original board.

Because everything is `board -> X`, combinators compose freely. `piece-at` doesn't take a square — it takes a `board -> square` combinator. That means you can pass `(coord 'e4)` for a static square, or `(king-coord side-to-move)` for a dynamic one:

```racket
(piece-at (coord 'e4))                 ; static square
(piece-at (king-coord side-to-move))   ; square computed from the board
```

## File Layout

```
Chess/
├── board.rkt        ; structs, FEN parser, board-ref/set, sq primitives, piece-at
├── combinators.rkt  ; generic combinator algebra — no chess knowledge
├── movegen.rkt      ; legal move generation, attack detection
├── chess.rkt        ; chess-specific combinators; re-provides everything
├── main.rkt         ; example programs
└── tests/
    ├── test-board.rkt
    ├── test-combinators.rkt
    └── test-chess.rkt
```

## Data Structures (`board.rkt`)

```racket
(struct piece (color type) #:transparent)
; color: 'white | 'black
; type:  'pawn | 'rook | 'knight | 'bishop | 'queen | 'king

(struct square (file rank) #:transparent)
; file 0-7 (a=0..h=7), rank 0-7 (1st=0..8th=7)
; vector index = rank*8 + file

(struct move (from to promotion) #:transparent)
; promotion: #f or piece-type symbol

(struct board
  (squares         ; vector[64]: piece or #f
   to-move         ; 'white | 'black
   castling        ; castling-rights struct
   en-passant      ; square or #f
   halfmove-clock
   fullmove-number) #:transparent)
```

Key helpers: `(coord 'e4)` lifts an algebraic symbol to a `board -> square` combinator. `fen->board` parses any FEN string. `starting-board` is the standard opening position.

## Combinator Algebra (`combinators.rkt`)

No chess knowledge — pure higher-order functions over `board -> X`.

```racket
;; Control flow
(board-if   pred then else)   ; branches on pred, both branches see original board
(board-and  comb ...)         ; variadic, short-circuits, returns last truthy value
(board-or   comb ...)         ; variadic, short-circuits
(board-not  comb)

;; Lifting
(board-const   v)             ; always returns v, ignores board
(board-compose comb proc)     ; applies comb, feeds result into plain function proc
(board-lift2   f comb1 comb2) ; applies f to results of two combinators
(board-equal?  comb1 comb2)   ; (board-lift2 equal? ...)
(board-pipe    comb1 comb2)   ; comb1: board->board, comb2 queries new board

;; Collections
(board-map  list-comb f)      ; maps plain function f over list result
(board-seq  comb ...)         ; applies all combs to same board, returns list
```

### `board-if` semantics

```racket
(define (board-if pred-comb then-comb else-comb)
  (lambda (b)
    (if (pred-comb b)
        (then-comb b)    ; original board, not pred's result
        (else-comb b))))
```

Racket's native truthiness applies — any non-`#f` value is truthy. A piece struct is truthy, so `(board-if (piece-at (coord 'e4)) ...)` naturally means "if there is a piece on e4."

## Chess Combinators (`chess.rkt`)

Every argument is a combinator. Static values are lifted with `coord` or `board-const`.

```racket
;; Square combinators
(coord 'e4)                     ; board -> square  (static)
(king-coord color-comb)         ; board -> square  (dynamic — finds the king)

;; Piece query factories
(piece-at    sq-comb)           ; board -> piece|#f
(square-empty? sq-comb)         ; board -> bool
(moves-from  sq-comb)           ; board -> (listof move)
(apply-move  mv-comb)           ; board -> board

;; Combinators (not factories)
side-to-move                    ; board -> 'white|'black
opp-side                        ; board -> 'white|'black  (opponent of side-to-move)
in-check?                       ; board -> bool
legal-moves                     ; board -> (listof move)

;; Piece predicates (plain functions, use with board-compose)
pawn? rook? knight? bishop? queen? king-piece? white? black?
```

## Attack Detection (`movegen.rkt`)

Attack detection is itself expressed as a combinator chain. The only primitive with a loop is `ray-first-piece` — everything above it is pure composition:

```
ray-first-piece          ; sole primitive — slides until it hits a piece
    ↓ passed as p-comb
piece-matches?           ; board-and of p-comb, board-equal?(color), board-compose(pred)
    ↓
any-attacker?            ; apply board-or over a list of piece-combs
    ↓ fed by
slider-candidates        ; (ray-first-piece sq-comb dir) per direction
jump-candidates          ; (piece-at (sq-offset sq-comb delta-comb)) per delta
    ↓
attacked-by?             ; board-or over all piece types
    ↓
in-check?  =  (attacked-by? (king-coord side-to-move) opp-side)
```

`sq-offset` takes a `delta-comb` (`board -> (file . rank)`), not a plain pair. Static deltas are wrapped in `board-const` by `jump-candidates`. Dynamic deltas like pawn direction are combinator factories:

```racket
; (pawn-delta df) returns a factory: by-color-comb -> delta-comb
(define (pawn-delta df)
  (lambda (by-color-comb)
    (board-compose by-color-comb
                   (lambda (color) (cons df (if (eq? color 'white) -1 1))))))
```

## Example Programs

```racket
;; Is there a white pawn on e4?
(define white-pawn-on-e4?
  (board-if (piece-at (coord 'e4))
            (board-compose (piece-at (coord 'e4))
                           (lambda (p) (and (white? p) (pawn? p))))
            (board-const #f)))

;; Legal moves only when in check
(define escape-check-moves
  (board-if in-check? legal-moves (board-const '())))

;; Game state detectors
(define game-over?  (board-not (board-compose legal-moves pair?)))
(define checkmate?  (board-and in-check? game-over?))
(define stalemate?  (board-and (board-not in-check?) game-over?))

;; Apply the first legal move
(define apply-first-legal-move
  (board-if (board-compose legal-moves pair?)
            (apply-move (board-compose legal-moves car))
            (board-const #f)))

;; Piece at the moving side's king square — dynamic square composition
(define piece-on-king-square
  (piece-at (king-coord side-to-move)))
```

## Running

```bash
racket main.rkt                      # run demo
racket tests/test-board.rkt          # board struct tests
racket tests/test-combinators.rkt    # combinator algebra tests
racket tests/test-chess.rkt          # chess combinator tests
```

## Design Notes

- **Raw `(lambda (b) ...)` only in primitives** — `piece-at`, `sq-offset`, `ray-first-piece`, `king-coord`, `apply-move`. Everything above those is combinator composition.
- **Factories separate static config from combinator args** via currying — e.g. `(pawn-delta -1)` returns a factory taking `by-color-comb`, not a function taking both at once.
- **Truthiness as a feature** — predicates don't need to return strict booleans. `board-if` inherits Racket's truthiness so piece queries double as boolean guards.
- **`board-compose` is the lift** for threading a combinator's result into a plain function. For two combinators, use `board-lift2`.
