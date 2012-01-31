(ns tictactoe.test.main
  (:use tictactoe.core clojure.test tictactoe.ui.base tictactoe.ai
        [tictactoe.main :only [move! new-game!]])
  (:require [tictactoe.ui.mock :as mock])
  (:import [tictactoe.ui.mock MockGame]))

(def game-count 500)

(defstrategy easy [player board]
  (empty-cells board all-coords))

(defstrategy medium [player board]
  (winning-moves board (other-player player))
  (empty-cells board all-coords))

(def hard ai-recommended-move)

(def strategies [easy medium hard])

(deftest player-x-unbeatable
  "Plays a specified number of games against an AI playing X; the AI should never lose."
  
  (doall
    (for [strat strategies]
      (let [game (mock/create-game :o game-count move! strat)]
        (new-game! game)
        (is (= (reduce + @(:win-record game)) (:game-count game)))
        (is (zero? (@(:win-record game) 0)))))))
  

(deftest player-o-unbeatable
  "Plays a specified number of games against an AI playing X; the AI should never lose."      
  (for [strat strategies]
    (let [game (mock/create-game :x game-count move! strat)]
      (new-game! game)
      (is (= (reduce + @(:win-record game)) (:game-count game)))
      (is (zero? (@(:win-record game) 0))))))
