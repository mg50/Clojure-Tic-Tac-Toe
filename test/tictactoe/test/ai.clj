(ns tictactoe.test.ai
  (:use tictactoe.core tictactoe.ai clojure.test))

(defmacro set-function [& fn-names]
  "Takes tactic(s) `t` and returns a corresponding function `t*` which returns a hash set of moves."
  (letfn [(make-set-function [f]
                             (let [f* (symbol (str f "*"))]
                               `(defn- ~f* [& args#]
                                   (set (apply ~f args#)))))]
    `(do
       ~@(map make-set-function fn-names))))

(set-function winning-moves forking-moves atari-moves)


(deftest test-winning-moves
  (let [board [[0  :x :x]
               [0  :o :x]
               [0  :o :o]]]
    (is (= #{[0 0]} (winning-moves* board p-x)))
    (is (= #{[0 0] [0 2]} (winning-moves* board p-o))))) 


(deftest test-forking-moves
  (let [board1 [[ 0  0 :x]
                [ 0 :o  0]
                [:x  0  0]]
        
        board2 [[:x  0  0]
                [ 0 :x :o]
                [ 0  0 :o]]]
    (is (= #{[0 0] [2 2]} (forking-moves* board1 p-x)))
    (is (= #{[1 0] [0 2] [2 0]} (forking-moves* board2 p-x)))))


(deftest test-move-leads-to-fork?
  (let [board1 [[ 0  0 :x]
                [ 0 :o  0]
                [:x  0  0]]
        
        board2 [[ 0  0  0]
                [ 0 :o :x]
                [ 0 :x  0]]]
    
    (is (move-leads-to-fork? board1 p-o [0 0]))
    (is (move-leads-to-fork? board1 p-o [2 2]))
    (is (not (move-leads-to-fork? board1 p-o [1 0])))
    
    (is (move-leads-to-fork? board2 p-o [0 0]))))

(deftest test-atari-moves
  (let [board1 [[:x  0  0]
                [:o  0  0]
                [:x  0  0]]
    
        board2 [[ 0  0 :o]
                [ 0 :x  0]
                [:x  0  0]]]
        
        (is (= #{[1 0] [2 0] [1 1] [1 2] [2 2]} (atari-moves* board1 p-x)))
        (is (= #{[1 1]} (atari-moves* board1 p-o))) ;Moving [2 1] leads to a fork at [1 1]
    
        (is (= #{[0 0] [2 2]} (atari-moves* board2 p-o)))))