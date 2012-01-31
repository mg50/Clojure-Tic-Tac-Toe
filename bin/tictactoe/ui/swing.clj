(ns tictactoe.ui.swing
  (:use tictactoe.core 
        [tictactoe.util :only [map-matrix]]
        tictactoe.ui.base)
  (:import (java.awt Color Dimension Font)
           (javax.swing JPanel JFrame JOptionPane)
           (java.awt.event MouseListener MouseEvent)))

(def background-color Color/white)
(def line-color Color/black)
(def font (Font. "Serif" Font/PLAIN 90))
(def cell-size 100)
(def player-color {p-x Color/blue, p-o Color/red})
(def player-string {p-x "X", p-o "O"})

(defn alert [jpanel msg]
  (JOptionPane/showMessageDialog jpanel msg "Game Over" JOptionPane/PLAIN_MESSAGE))

(defn display-option [jpanel msg options]
  (JOptionPane/showOptionDialog jpanel
                                msg
                                "New Game"
                                JOptionPane/YES_NO_OPTION
                                JOptionPane/QUESTION_MESSAGE
                                nil
                                (to-array options)
                                (first options)))

(defmacro create-option-dialog [fn-name msg options option-values]
  "Macro to create a Swing option dialog for a given JPanel."
  `(defn ~fn-name [panel#]
     (if (zero? (display-option panel# ~msg ~options))
       ~(first option-values)
       ~(second option-values))))

(create-option-dialog start-new-game-dialog
                      "Start a new game?"
                      ["Yes" "No"]
                      [true false])

(create-option-dialog play-vs-ai-dialog
                      "Play vs. AI?"
                      ["Yes" "No"]
                      [true false])

(create-option-dialog which-player-dialog
                      "Select side to play as:"
                      ["X" "O"]
                      [p-x p-o])


(defn paint-tile
  "Fills in an individual tile with an X or an O. Used in all redraws. g is a Swing graphics component 
  object; this method is used to override its paintComponent method."
  [g [x y] player]
  (when (not= empty-cell player)
    (let [string (player-string player)
          metrics (.getFontMetrics g)
          text-size (.getStringBounds metrics string g)
          half-text-width (/ (.getWidth text-size) 2)
          half-cell-size (/ cell-size 2)
          half-text-height (* 2/6 (.getHeight text-size))
          center-x (int (+ (* x cell-size)
                           (- half-cell-size half-text-width)))
          center-y (int (+ (* y cell-size)
                           half-text-height
                           half-cell-size))]
      (doto g
        (.setColor (player-color player))
        (.drawString string center-x center-y)))))

(defn create-jpanel [game move-callback!]
  "Creates the JPanel proxy for the game, attaching to it the supplied move callback as a click event."
  (let [cell-size*2 (* cell-size 2)
        cell-size*3 (* cell-size 3)]
    (proxy [JPanel MouseListener] []
      (paintComponent [g]
        (proxy-super paintComponent g)
        (doto g
          (.setFont font)
          (.setColor background-color)
          (.fillRect 0 0 cell-size*3 cell-size*3)
          (.setColor line-color)
          (.drawLine 0 cell-size cell-size*3 cell-size)
          (.drawLine 0 cell-size*2 cell-size*3 cell-size*2)
	        (.drawLine cell-size 0 cell-size cell-size*3)
	        (.drawLine cell-size*2 0 cell-size*2 cell-size*3))
        (map-matrix @(:board game) #(paint-tile g [%2 %3] %1)))
      (getPreferredSize []
        (Dimension. cell-size*3 cell-size*3))
      (mouseClicked [e] 
        (let [get-ordinate #(-> % (/ cell-size) Math/floor int)
              x (get-ordinate (.getX e))
              y (get-ordinate (.getY e))]
          (move-callback! game [x y])))
      (mousePressed [e])
      (mouseReleased [e])
      (mouseEntered [e])
      (mouseExited [e]))))

(defn create-ui [game move-callback!]
  "Creates the JFrame, attaches a JPanel to it and makes the JFrame visible. Returns the JPanel."
  (let [frame (JFrame. "Tic-Tac-Toe")
        panel (create-jpanel game move-callback!)]
    (.addMouseListener panel panel)
    (doto frame
      (.add panel)
      (.setResizable false)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.pack)
      (.setVisible true))
    panel))

(defrecord SwingGame [board game-running ai current-player first-game jpanel] 
  UI
  (update-ui [this] (.repaint @jpanel))
  (victory-message [this victor]
    (cond
      (= victor p-x) (alert
                       @jpanel 
                       (str "Player " (player-string p-x) " has won!"))
      (= victor p-o) (alert 
                       @jpanel
                       (str "Player " (player-string p-o) " has won!"))
      :else (alert 
              @jpanel
              "The game ended in a draw!")))
  (start-new-game? [this] (start-new-game-dialog @jpanel))
  (play-vs-ai? [this] (play-vs-ai-dialog @jpanel))
  (which-player [this] (which-player-dialog @jpanel))
  (prompt-move [this])) ;No-op

(defn create-game [move-callback]
  "Creates a game record as well as a JPanel. Afterwards, the panel is attached to the game via an atom."
  (let [game (SwingGame. (atom nil) (atom nil) (atom nil) (atom nil) (atom nil) (atom nil)) 
        panel (create-ui game move-callback)]
    (reset! (:jpanel game) panel)
    game))

