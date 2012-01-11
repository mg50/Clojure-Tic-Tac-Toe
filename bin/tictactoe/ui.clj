(ns tictactoe.ui
  (:use tictactoe.core)
  (:import (java.awt Color Dimension Font)
           (javax.swing JPanel JFrame JOptionPane)
           (java.awt.event MouseListener MouseEvent)))

(def background-color Color/white)
(def line-color Color/black)

(def font (Font. "Serif" Font/PLAIN 90))

(def cell-size 100)
(def cell-size*2 (* 2 cell-size))
(def cell-size*3 (* 3 cell-size))

(defn get-color [player]
  (cond
    (= player p-x) Color/blue 
    (= player p-o) Color/red))

(defn get-string [player]
  (cond
    (= player p-x) "X"
    (= player p-o) "O"))

(defn victory-message
  "Displays a Swing dialog announcing a winner (or a draw, if no winner is supplied)."
  ([frame victor]
    (letfn [(dialog [msg]
              (JOptionPane/showMessageDialog frame msg "Game Over" JOptionPane/PLAIN_MESSAGE))]
      (cond
        (= victor p-x) (dialog (str "Player " (get-string p-x) " has won!"))
        (= victor p-o) (dialog (str "Player " (get-string p-o) " has won!"))
        :else (dialog "The game ended in a draw!"))))
  ([frame]
    (victory-message frame nil)))
  

(defn display-option [frame msg options]
  (JOptionPane/showOptionDialog frame
                                msg
                                "New Game"
                                JOptionPane/YES_NO_OPTION
                                JOptionPane/QUESTION_MESSAGE
                                nil
                                (to-array options)
                                (first options)))

(defmacro create-option-dialog [frame fn-name msg options]
  "Macro to create a Swing option dialog for a given JFrame. Used just to make syntax less cluttered."
  `(defn ~fn-name []
     (display-option ~frame ~msg ~options)))

(defn paint-tile
  "Fills in an individual tile with an X or an O. Used in all redraws. g is a Swing graphics component 
  object; this method is used to override its paintComponent method."
  [g [x y] player]
  (when (not= empty-cell player)
    (let [string (get-string player)
          metrics (.getFontMetrics g)
          text-size (.getStringBounds metrics string g)
          text-width (.getWidth text-size)
          text-height (* 2/3 (.getHeight text-size))
          center-x (int (+ (* x cell-size)
                           (- (/ cell-size 2) (/ text-width 2))))
          center-y (int (+ (* y cell-size)
                           (/ text-height 2)
                           (/ cell-size 2)))]
      (doto g
        (.setColor (get-color player))
        (.drawString string center-x center-y)))))

(defn game-panel [board-atom move-callback!]
  "Creates the JPanel proxy for the gameboard, attaching to it the supplied move callback as a click event."
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
      (mapmatrix @board-atom #(paint-tile g [%2 %3] %1)))
    (getPreferredSize []
      (Dimension. (* 3 cell-size) (* 3 cell-size)))
    (mouseClicked [e] 
      (let [mouse-x (.getX e),
            x (-> mouse-x (/ cell-size) Math/floor int),
            mouse-y (.getY e),
            y (-> mouse-y (/ cell-size) Math/floor int)]
        (move-callback! this [x y])))
    (mousePressed [e])
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])))

(defn create-game [board-atom move-callback!]
  "Creates the JFrame. Also creates game option dialogs and attaches them to the JFrame."
  (let [frame (JFrame. "Tic-Tac-Toe")
        panel (game-panel board-atom move-callback!)]
    (.addMouseListener panel panel)
    (create-option-dialog frame start-new-game-dialog 
                          "Start a new game?"
                          ["Yes" "No"])
	  
    (create-option-dialog frame play-vs-ai-dialog
                          "Play vs. AI?"
                          ["Yes" "No"])
	  
    (create-option-dialog frame which-player-dialog
                          "Select side to play as:"
                          ["X" "O"])
    (doto frame
      (.add panel)
      (.setResizable false)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.pack)
      (.setVisible true))
    panel))