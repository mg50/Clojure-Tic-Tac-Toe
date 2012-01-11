(ns tictactoe.core)

(def empty-board [[0 0 0]
                  [0 0 0]
                  [0 0 0]])

(def p-x :x)
(def p-o :o)
(def empty-cell 0)

(defn cell-empty? [board x y]
  (= empty-cell (get-in board [y x])))

(defn other-player [p]
  (cond
    (= p p-x) p-o
    (= p p-o) p-x))

(defn rows [board] 
  "Returns a board's rows as vectors."
  board)

(defn columns [board]
  "Return a board's columns as vectors."
  (apply map vector board))

(defn diagonals [board]
  "Return a board's diagonals as vectors. Note: the top-left-to-bottom-right diagonal is always first."
  (let [left-diag (map-indexed #(%2 %1) board),
        right-diag (map-indexed #(%2 (- 2 %1)) board)]
    [left-diag right-diag]))

(defn lines [board]
  (concat (rows board) (columns board) (diagonals board)))

(defn player-won? [board player]
  "Checks to see if a given player has a three-in-a-row on a board."
  (let [lines (lines board),
        winning-vector (repeat 3 player)]
    (if (some #(= winning-vector %) lines)
      true
      false)))

(defn board-full? [board]
  (if ((set (flatten board)) empty-cell)
    false
    true))

(defn pick-cell 
  "A board is a vector of row vectors; thus we access a cell with coordinates (x, y) via (get-in board [y x]).
  This is a convenience method that allows us to more naturally access a cell with [x y]."
  ([board x y]
    (get-in board [y x]))
  ([board [x y]]
    (pick-cell board x y)))

(defn mapmatrix [matrix f]
  "Takes a two-dimensional vector and maps it to a new two-dimensional vector based on the coordinates of
  each cell together with the value of that cell. Thus, f takes three arguments: the cell-value, x and y.
  Borrowed from Alex Yukashev's guide to writing Tetris in Clojure."
  (into [] (map-indexed (fn [y row]
                          (into [] (map-indexed (fn [x el]
                                                  (f el x y))
                                                row)))
                        matrix)))

(defn next-board [board player [new-x new-y]]
  "Takes a board, player and move coordinate and tries to return a new board with the player having
  moved at that coordinate."
  (when (= empty-cell (pick-cell board new-x new-y))
    (assoc-in board [new-y new-x] player)))
