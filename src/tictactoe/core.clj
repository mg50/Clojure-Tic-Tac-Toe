(ns tictactoe.core)

(def empty-board [[0 0 0]
                  [0 0 0]
                  [0 0 0]])

(def p-x :x)
(def p-o :o)
(def empty-cell 0)
(def other-player {p-o p-x, p-x p-o})

(def center [1 1])
(def corners [[0 0], [0 2], [2 0], [2 2]])
(def sides [[0 1], [2 1], [1 2], [1 0]])
(def all-coords (for [i (range 3), j (range 3)]
                  [i j]))

(defn pick-cell 
  "A board is a vector of row vectors; thus we access a cell with coordinates (x, y) via (get-in board [y x]).
  This is a convenience method that allows us to more naturally access a cell with [x y]."
  ([board x y]
    (get-in board [y x]))
  ([board [x y]]
    (pick-cell board x y)))

(defn cell-empty? 
  ([board [x y]]
    (let [cell-val (pick-cell board x y)]
      (if (= empty-cell cell-val)
        [x y]
        nil)))
  ([board x y]
    (cell-empty? board [x y])))
         

(def row-coords
  (doall
    (for [col (range 3)]
      (for [row (range 3)]
        [row col]))))

(def col-coords
  (doall
    (for [row (range 3)] 
      (for [col (range 3)]
        [row col]))))

(def diag-coords
  (doall
    [(for [i (range 3)] [i i])
     (for [i (range 3)] [(- 2 i) i])]))

(def line-coords
  (concat row-coords col-coords diag-coords))

(defn line-freqs [board line]
  (frequencies (map #(pick-cell board %) line)))

(defn empty-coord-in-line [board line]
  (some #(when (cell-empty? board %) %) line))


(defn find-victor [board]
  (let [all-line-freqs (map #(line-freqs board %) line-coords)
        player-won? (fn [player]
                      (some #(= % {player 3}) all-line-freqs))]
    (cond
      (player-won? p-x) p-x
      (player-won? p-o) p-o
      (some #(= empty-cell %) (flatten board)) nil
      :else :draw)))

(defn next-board [board player [new-x new-y]]
  "Takes a board, player and move coordinate and tries to return a new board with the player having
  moved at that coordinate."
  (when (= empty-cell (pick-cell board new-x new-y))
    (assoc-in board [new-y new-x] player)))
