(ns tictactoe.ui.console
  (:use tictactoe.core
        [tictactoe.util :only [map-matrix]]
        tictactoe.ui.base
        [clojure.string :only [join]]))

(def strings {p-x "X", p-o "O", empty-cell " "})

(defn print-board [board]
  "Prints out a text-representation of a game board."
  (let [string-board (map-matrix board (fn [x _ _] (strings x)))
        rows (map #(join "|" %) string-board)]
    (println (join "\n" (interpose "-----" rows)) "\n")))

(def coords {"top left" [0 0]
             "top middle" [1 0]
             "top right" [2 0]
             "middle left" [0 1]
             "center" [1 1]
             "middle right" [2 1]
             "bottom left" [0 2]
             "bottom middle" [1 2]
             "bottom right" [2 2]})

(defn alert [& msgs]
  (apply println "\n" msgs))

(defn dialog [msg yes no]
  (print (str msg " y/n? "))
  (flush)
  (case (read-line)
    "y" yes
    "n" no
    (recur msg yes no)))

(defn read-move []
  "Translates text input to a coordinate on the game board. The coordinate must be empty."
  (print "Please enter your move (or 'help' for options): ")
  (flush)
  (let [input (read-line)]
    (if (= input "help")
      (do
        (alert "Type in one of the following: top left, top middle, top right, middle left, " 
               "center, middle right, bottom left, bottom middle, bottom right.")
        (recur))
     (if-let [move (coords input)]
       move
       (recur)))))

(defrecord ConsoleGame [board game-running ai current-player first-game move-callback]
  UI
  (update-ui [this] (print-board @board))
  (victory-message [this victor]
    (cond
      (= victor p-x) (alert (str "Player " (strings p-x) " has won!"))
      (= victor p-o) (alert (str "Player " (strings p-o) " has won!"))
      :else (alert "The game ended in a draw!")))
  (start-new-game? [this] (dialog "Start a new game?" true false))
  (play-vs-ai? [this] (dialog "Play vs. AI?" true false))
  (which-player [this] (dialog "Play as X?" p-x p-o))
  (prompt-move [this]
    (let [move (read-move)]
      (if (cell-empty? @board move)
        (move-callback this move)
        (do
          (print "Square already occupied. ")
          (recur))))))

(defn create-game [move-callback]
  (ConsoleGame. (atom nil) (atom nil) (atom nil) (atom nil) (atom nil) move-callback))
