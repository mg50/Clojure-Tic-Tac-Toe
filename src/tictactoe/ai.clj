(ns tictactoe.ai
  (:use tictactoe.core 
        [tictactoe.util :only [or-non-empty]]))

(defmacro deftactic [tactic-name args & body]
  "A tactic is a function which returns a (possibly empty) group of coordinates for valid moves."
  `(defn ~tactic-name ~args
     (keep identity (do ~@body))))


(defmacro defstrategy [strat-name [player board] & tactics]
  (let [tactics* (map (fn [t]
                        `(let [result# ~t] (when-not (empty? result#) result#)))
                      tactics)]
  `(defn ~strat-name [~player ~board]
     (rand-nth
       (or ~@tactics*)))))

(deftactic winning-moves [board player]
  "Finds moves that lead to a three-in-a-row for a player."
  (for [line line-coords :when (= (line-freqs board line) {player 2, empty-cell 1})]
    (empty-coord-in-line board line)))

(declare move-leads-to-fork?)
(deftactic atari-moves [board player]
  "Finds ataris, moves that lead to a two-in-a-row that threaten to win the game in the next turn. However,
  it avoids ataris that lead to getting forked immediately after by the opponent."
  (for [line line-coords :when (= {player 1, empty-cell 2} (line-freqs board line))
        coord line :when (and
                           (cell-empty? board coord)
                           (not (move-leads-to-fork? board player coord)))]
    coord))

(declare lines-forkable)
(deftactic forking-moves [board player]
	(for [l1 line-coords, l2 line-coords :when (< (.indexOf line-coords l1)
	                                              (.indexOf line-coords l2))]
	  (lines-forkable l1 l2 board player)))
  
(deftactic empty-cells [board cells]
  (filter #(cell-empty? board %) cells))

(defstrategy ai-recommended-move [player board]
  (winning-moves board player)
  (winning-moves board (other-player player))
  (forking-moves board player)
  (atari-moves board player)
  (empty-cells board [center])
  (empty-cells board corners)
  (empty-cells board sides))



(defn lines-forkable [l1 l2 board player]
	(when-let [intersect (some (set l1) l2)]
	  (when (and
	          (cell-empty? board intersect)
	          (= {player 1, empty-cell 2}
	             (line-freqs board l1)
	             (line-freqs board l2)))
	    intersect)))

(defn move-leads-to-fork? [board player move]
  "Looks ahead one step to see if a given move can lead to the opponent setting up a fork."
  (let [board* (next-board board player move)
        winning-moves (winning-moves board* player)
        forks (forking-moves board* (other-player player))]
    (when forks
      (every? (set forks) winning-moves))))