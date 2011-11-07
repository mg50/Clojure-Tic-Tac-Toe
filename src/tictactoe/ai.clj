(ns tictactoe.ai
  (:use tictactoe.tictactoe)
  (:gen-class))

(defn analyze-lines
  ([line line-type line-num f] ;If f is truthy on line, return coords (based on line type) 
    (if-let [result (f line)]
      (case line-type
        :row [result line-num]
        :column [line-num result]
        :diagonal (if (zero? line-num)
                    [result result]
                    [(- 2 result) result]))))
  ([board f]
    (let [rows (rows board),
          rows-analyzed (map-indexed #(analyze-lines %2 :row %1 f) rows),  
          columns (columns board),
          columns-analyzed (map-indexed #(analyze-lines %2 :column %1 f) columns),
          diagonals (diagonals board),
          diagonals-analyzed (map-indexed #(analyze-lines %2 :diagonal %1 f) diagonals),
          analyzed (concat rows-analyzed columns-analyzed diagonals-analyzed)]
      (some #(when-not (nil? %) %) analyzed))))

(defn find-winning-move [player board]
  (analyze-lines board (fn [line]
                         (when (= (frequencies line) {empty-cell 1, player 2})
                           (.indexOf (vec line) empty-cell)))))

(defn block-losing-move [player board]
  (find-winning-move (other-player player) board))

(defn atari [player board]
  (analyze-lines board (fn [line]
                         (when (= (frequencies line) {empty-cell 2, player 1})
                           (.indexOf (vec line) empty-cell)))))

(defn center [board]
  (when (cell-empty? board 1 1)
    [1 1]))

(defn random-empty-corner [board]
  (let [coords [[0 0], [0 2], [2 0], [2 2]]
        empty-coords (filter #(= empty-cell (pick-cell board %)) coords)]
    (when-not (empty? empty-coords)
      (rand-nth empty-coords))))

(defn random-empty-cell [board]
  (let [coords (for [i (range 3), j (range 3)] 
                 (pick-cell i j)),
        empty-coords (filter #(= empty-cell (pick-cell board %)) coords)]
    (when-not (empty? empty-coords)
      (rand-nth empty-coords))))

(defn ai-recommended-move [player board]
  "Returns coords of AI's next move"
  (or
    (find-winning-move player board) ;Play winning move if available
    (block-losing-move player board) ;Block opponent's winning move if possible
    (atari player board)
    (center board)
    (random-empty-corner board)
    (random-empty-cell board)))