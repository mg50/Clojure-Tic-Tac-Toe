(ns tictactoe.tictactoe)

(def board [[0 0 0]
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
  board)

(defn columns [board]
  (apply map vector board))

(defn diagonals [board]
  (let [left-diag (map-indexed #(%2 %1) board),
        right-diag (map-indexed #(%2 (- 2 %1)) board)]
    [left-diag right-diag]))

(defn lines [board]
  (concat (rows board) (columns board) (diagonals board)))

(defn player-won? [board player]
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
  ([board x y]
    (get-in board [y x]))
  ([board [x y]]
    (pick-cell board x y)))

(defn mapmatrix [matrix f]
  (into [] (map-indexed (fn [y row]
                          (into [] (map-indexed (fn [x el]
                                                  (f el x y))
                                                row)))
                        matrix)))

(defn next-board [board kwd [newx newy]]
  (when (= empty-cell (pick-cell board newx newy))
    (mapmatrix board (fn [el x y]
                       (if (= [newx newy] [x y])
                         kwd
                         el)))))

