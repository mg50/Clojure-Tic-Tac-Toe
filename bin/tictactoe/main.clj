(ns tictactoe.main
  (:use tictactoe.core
        [tictactoe.ui :only [create-game start-new-game-dialog play-vs-ai-dialog which-player-dialog 
                             victory-message]]
        [tictactoe.ai :only [ai-recommended-move]])
  (:gen-class))


(def *board* (atom nil))
(def *current-player* (atom nil))
(def *ai* (atom nil))
(def *game-running* (atom false))
(def *first-game* (atom true))

(declare move!)
(defn new-game! [game]
  "Starts a fresh game, repainting the UI to be empty. Displays game option dialogs. Also, makes first move
  for the AI if the AI is player X."
  (when (or @*first-game* 
            (zero? (start-new-game-dialog)))
    (reset! *first-game* false)
    (reset! *board* empty-board)
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
  "If the cell at supplied coordinates is empty, this method updates the board atom storing the board's state
  and checks whether a win/loss/draw has occurred. If so, displays the relevant dialogs to start a new game.
  Otherwise, switches players and repeats if necessary (i.e., if the AI moves)."
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
                (when (= @*current-player* @*ai*)
                  (recur game (ai-recommended-move @*ai* @*board*))))))))


(defn -main []
  (let [game (create-game *board* move!)]
    (new-game! game)))
