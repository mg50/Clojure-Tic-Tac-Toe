(ns tictactoe.ui.base)

(defprotocol UI
  (update-ui [this])
  (victory-message [this victor])
  (start-new-game? [this])
  (play-vs-ai? [this])
  (which-player [this])
  (prompt-move [this]))