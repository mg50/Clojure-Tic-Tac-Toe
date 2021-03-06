(ns tictactoe.ui.mock
  (:use tictactoe.core tictactoe.ui.base))


(defrecord MockGame [board game-running ai current-player first-game player game-count win-record 
                     move-callback strategy]
  UI
  (update-ui [this])
  (victory-message [this victor]
    (cond
      (= player victor) (swap! win-record #(update-in % [0] inc))
      (= (other-player player) victor) (swap! win-record #(update-in % [1] inc))
      :else (swap! win-record #(update-in % [2] inc))))
  (start-new-game? [this] 
    (let [[wins losses draws] @win-record]
      (not= game-count (+ wins losses draws))))
  (play-vs-ai? [this] true)
  (which-player [this] player)
  (prompt-move [this] ;Play a move at random
    (let [move (strategy player @board)]
      (move-callback this move))))
  
  (defn create-game [player game-count move-callback strategy]
    (MockGame. (atom nil) (atom nil) (atom nil) (atom nil) (atom nil) player 
               game-count (atom [0 0 0]) move-callback strategy))