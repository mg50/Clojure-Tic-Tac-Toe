(ns tictactoe.ai
  (:use tictactoe.core 
        [tictactoe.util :only [or-non-empty]]))


(defn winning-moves [board player]
  "Finds moves that lead to a three-in-a-row for a player."
  (for [line line-coords :when (= (line-freqs board line) {player 2, empty-cell 1})]
    (empty-coord-in-line board line)))

(declare move-leads-to-fork?)
(defn atari-moves [board player]
  "Finds ataris, moves that lead to a two-in-a-row that threaten to win the game in the next turn. However,
  it avoids ataris that lead to getting forked immediately after by the opponent."
  (for [line line-coords :when (= {player 1, empty-cell 2} (line-freqs board line))
        coord line :when (and
                           (cell-empty? board coord)
                           (not (move-leads-to-fork? board player coord)))]
    coord))

(defn forking-moves 
  "Finds all moves that create a fork - but ignores forks that already have two marks in one line, since these
  directly lead to a winning move anyway."
  ([board player]
    (for [l1 line-coords, l2 line-coords :when (< (.indexOf line-coords l1)
                                                  (.indexOf line-coords l2))]
      (forking-moves l1 l2 board player)))
  ([l1 l2 board player] ;Checks if two given lines are forkable
    (when-let [intersect (some (set l1) l2)]
      (when (and
              (cell-empty? board intersect)
              (= {player 1, empty-cell 2}
                 (line-freqs board l1)
                 (line-freqs board l2)))
        intersect))))

(defn empty-cells [board cell-groups]
  "Takes a list of groups of coordinates. Returns the first group that has an empty coordinate after
  filtering out the non-empty coordinates."
  (when cell-groups
    (let [empty-cell-coords (filter #(cell-empty? board %) (first cell-groups))]
      (if (empty? empty-cell-coords)
        (recur board (next cell-groups))
        empty-cell-coords))))

(defn ai-recommended-move [player board]
  (rand-nth
    (or-non-empty
      (winning-moves board player)
      (winning-moves board (other-player player)) ;Block losing moves
      (forking-moves board player)
      (atari-moves board player)
      (empty-cells board [[center]
                          corners
                          sides]))))
  

(defn move-leads-to-fork? [board player move]
  "Looks ahead one step to see if a given move can lead to the opponent setting up a fork."
  (let [board* (next-board board player move)
        winning-moves (winning-moves board* player)
        forks (forking-moves board* (other-player player))]
    (when forks
      (every? (set forks) winning-moves))))