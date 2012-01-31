(ns tictactoe.test.main
  (:use tictactoe.core clojure.test tictactoe.ui.base tictactoe.ai
        [tictactoe.main :only [move! new-game!]])
  (:require [tictactoe.ui.mock :as mock])
  (:import [tictactoe.ui.mock MockGame]))

(def game-count 500)

(defstrategy easy [player board] ;Plays a move randomly
  (empty-cells board all-coords))

(defstrategy medium [player board] ;Plays a move randomly (though tries to block losses)
  (winning-moves board (other-player player))
  (empty-cells board all-coords))

(def hard ai-recommended-move)

(deftest ai-unbeatable
  "Plays a specified number of games against an AI playing X; the AI should never lose."
  
  (doall
    (for [strategy [easy medium hard], player [p-x p-o]]
      (let [game (mock/create-game player game-count move! strategy)]
        (new-game! game)
        (is (= (reduce + @(:win-record game)) (:game-count game)))
        (is (zero? (@(:win-record game) 0)))))))
  
