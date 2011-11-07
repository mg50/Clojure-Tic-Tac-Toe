(ns tictactoe.game
  (:use tictactoe.tictactoe)
  (:import (java.awt Color Dimension BorderLayout Font)
           (javax.swing JPanel JFrame JOptionPane JButton JLabel)
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
  `(defn ~fn-name []
     (display-option ~frame ~msg ~options)))

(defn paint-tile
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
  "Creates the JPanel for the gameboard."
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
      (.setSize 300 320)
      (.setVisible true))
    panel))