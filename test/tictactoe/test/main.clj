(ns tictactoe.test.main
  (:use tictactoe.core clojure.test tictactoe.ui.base
        [tictactoe.main :only [move! new-game!]])
  (:require [tictactoe.ui.mock :as mock])
  (:import [tictactoe.ui.mock MockGame]))

(def game-count 500)

(deftest player-x-unbeatable
  "Plays a specified number of games against an AI playing X; the AI should never lose."
  (let [game (mock/create-game :o game-count move!)]
    
    (new-game! game)
    (is (zero? (@(:win-record game) 0)))))

(deftest player-o-unbeatable
  "Plays a specified number of games against an AI playing X; the AI should never lose."  
  (let [game (mock/create-game :x game-count move!)]
    
    (new-game! game)
    (is (zero? (@(:win-record game) 0)))))