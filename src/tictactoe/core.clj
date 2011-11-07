(ns tictactoe.core
  (:use tictactoe.tictactoe
        tictactoe.game
        tictactoe.ai)
  (:gen-class))


(def *board* (atom nil))
(def *current-player* (atom nil))
(def *ai* (atom nil))
(def *game-running* (atom false))
(def *first-game* (atom true))

(declare move!)
(defn new-game! [game]
  (when (or @*first-game* 
            (zero? (start-new-game-dialog)))
    (reset! *first-game* false)
    (reset! *board* board)
    (reset! *current-player* p-x)
    (reset! *game-running* true)
    (if (zero? (play-vs-ai-dialog))
      (reset! *ai* (if (zero? (which-player-dialog))
                     p-o
                     p-x))
      (reset! *ai* nil))
    (when (= p-x @*ai*) ;Make first move on behalf of AI if AI is X
      (move! game (ai-recommended-move @*ai* @*board*)))
    (.repaint game)))


(defn move! [game [x y]]
  (when @*game-running*
	  (when-let [next-board (next-board @*board* @*current-player* [x y])]
	    (reset! *board* next-board)
      (.repaint game)
	    (cond
        (player-won? @*board* @*current-player*) (do
                                                   (reset! *game-running* false)
                                                   (victory-message game @*current-player*)
                                                   (new-game! game))
        (board-full? @*board*) (do
                                 (reset! *game-running* false)
                                 (victory-message game)
                                 (new-game! game))
        :else (do
                (swap! *current-player* other-player)
                (if (= @*current-player* @*ai*)
                  (recur game (ai-recommended-move @*ai* @*board*))
                  (.repaint game)))))))


(defn -main []
  (let [game (create-game *board* move!)]
    (new-game! game)))
