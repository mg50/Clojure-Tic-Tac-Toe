(ns tictactoe.ai
  (:use tictactoe.core))

(defn analyze-lines
  "Tests rows, columns and diagonals - in that order - of a given board with a given function f. 
  Each row/column/diagonal is treated as a vector and referred to as a 'line.' f should return 
  either nil or some index of the line. As soon as f finds a line on which it's non-nil, analyze-lines figures
  out which square on the board corresponds to that line/index combination and returns the coordinates
  of that square. For example, if f returns nil on all the rows but returns 2 on the first column, this
  corresponds to the bottom-left square and so we return [0 2]."
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
      (some identity analyzed))))

(defn find-winning-move [player board]
  "Return the coordinates of a winning move, if one exists."
  (analyze-lines board (fn [line]
                         (when (= (frequencies line) {empty-cell 1, player 2})
                           (.indexOf (vec line) empty-cell)))))

(defn block-losing-move [player board]
  "Return the coordinates of a winning move the other player can make, if one exists."
  (find-winning-move (other-player player) board))

(defn atari [player board]
  "Return the first coordinate of an atari - a move that makes two-in-a-row 
  with the remaining square empty."
  (analyze-lines board (fn [line]
                         (when (= (frequencies line) {empty-cell 2, player 1})
                           (.indexOf (vec line) empty-cell)))))

(defn center [board]
  "Returns coordinates of the center cell if empty."
  (when (cell-empty? board 1 1)
    [1 1]))

(defn random-empty-corner [board]
  "Returns coordinates of an empty corner square at random if one exists."
  (let [coords [[0 0], [0 2], [2 0], [2 2]]
        empty-coords (filter #(= empty-cell (pick-cell board %)) coords)]
    (when-not (empty? empty-coords)
      (rand-nth empty-coords))))

(defn random-empty-cell [board]
  "Returns coordinates of any empty square at random if one exists."
  (let [coords (for [i (range 3), j (range 3)] 
                 [i j]),
        empty-coords (filter #(= empty-cell (pick-cell board %)) coords)]
    (when-not (empty? empty-coords)
      (rand-nth empty-coords))))

(defn ai-recommended-move [player board]
  "Returns coords of AI's next move. The AI prioritizes sub-strategies as defined in the function body.
  For instance, it first looks if it can make a winning move, and if so, returns the coordinates of that move.
  If not, it looks to see if the opponent is about to win with a two-in-a-row and tries to block that. And
  so forth. It can be proven that this strategy never loses."
  (or
    (find-winning-move player board) ;Play winning move if available
    (block-losing-move player board) ;Block opponent's winning move if possible
    (atari player board)
    (center board)
    (random-empty-corner board)
    (random-empty-cell board)))