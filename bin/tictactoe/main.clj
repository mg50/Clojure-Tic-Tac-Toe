(ns tictactoe.main
  (:use tictactoe.core
        [tictactoe.ai :only [ai-recommended-move]]
        [tictactoe.util :only [with-accessors]]
        tictactoe.ui.base)
  (:require [tictactoe.ui.swing :as swing]
            [tictactoe.ui.console :as console] :reload)
  (:import [tictactoe.ui.swing SwingGame]
           [tictactoe.ui.console ConsoleGame])
  (:gen-class))


(declare move!)
(defn new-game! [game]
  "Starts a fresh game, repainting the UI to be empty. Displays game option dialogs. Also, makes first move
  for the AI if the AI is player X."
  (with-accessors game [board ai first-game]
    (when (or @first-game
              (start-new-game? game))
      (reset! first-game false)
      (reset! board empty-board)
      (reset! (:current-player game) p-x)
      (reset! (:game-running game) true)
      (if (play-vs-ai? game)
        (reset! ai (if (= p-x (which-player game))
                     p-o
                     p-x))
        (reset! ai nil))
      (if (= p-x @ai) ;Make first move on behalf of AI if AI is X
        (move! game (ai-recommended-move @ai @board))
        (do
          (update-ui game)
          (prompt-move game))))))

(defn move! [game [x y]]
  "If the cell at supplied coordinates is empty, this method updates the board atom storing the board's state
  and checks whether a win/loss/draw has occurred. If so, displays the relevant dialogs to start a new game.
  Otherwise, switches players and repeats if necessary (i.e., if the AI moves)."
  (with-accessors game [board ai game-running current-player]
    (when @game-running
      (when-let [next-board (next-board @board @current-player [x y])]
        (reset! board next-board)
        (update-ui game)
        (if-let [victor (find-victor @board)]
          (do
            (reset! game-running false)
            (victory-message game victor)
            (new-game! game))
          (do
            (swap! current-player other-player)
            (if (= @current-player @ai)
              (recur game (ai-recommended-move @ai @board))
              (prompt-move game))))))))


(defn -main [& args]
  (let [game (if (= "console" (first args))
               (console/create-game move!)               
               (swing/create-game move!))]
    (reset! (:first-game game) true)
    (new-game! game)))
