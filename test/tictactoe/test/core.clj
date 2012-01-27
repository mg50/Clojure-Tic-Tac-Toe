(ns tictactoe.test.core
  (:use [tictactoe.core])
  (:use [clojure.test]))


(deftest basics
  (let [board [[ 0 :x :o]
               [:x  0  0]
               [:o :x :o]]]
    (is (= :x (pick-cell board [1 0])))
    (is (= :o (pick-cell board 2 2)))
    (is (cell-empty? board [2 1]))))

(deftest test-line-freqs
  (let [board [[ 0 :x :o]
               [:x  0  0]
               [:o :x :o]]]
    (is (= (map #(line-freqs board %) line-coords)
           [{:x 1, :o 1, 0 1}
            {:x 1, 0 2}
            {:o 2, :x 1}
            {:x 1, :o 1, 0 1}
            {:x 2, 0 1}
            {:o 2, 0 1}
            {:o 1, 0 2}
            {:o 2, 0 1}]))
    
    (is (= (map #(empty-coord-in-line board %) line-coords)
           [[0 0]
            [1 1]
            nil
            [0 0]
            [1 1]
            [2 1]
            [0 0]
            [1 1]]))))

(deftest test-find-victor
  (let [b1 [[:x :x :x]
            [ 0 :o :o]
            [:o :x  0]]
        
        b2 [[ 0 :x :o]
            [:x  0  0]
            [:o :x :o]]
    
        b3 [[:x :o :x]
            [:o :x :o]
            [:o :x :o]]
        
        b4 [[:x  0 :o]
            [:x :o  0]
            [:o :x :x]]]
        
    (is (= :x (find-victor b1)))
    
    (is (nil? (find-victor b2)))
    
    (is (= :draw (find-victor b3)))
    
    (is (= :o (find-victor b4)))))


(deftest test-next-board
  (let [b1 [[ 0 :x :o]
            [:x  0  0]
            [:o :x :o]]
        
        b1-x [[:x :x :o]
              [:x  0  0]
              [:o :x :o]]
    
        b1-o [[:o :x :o]
              [:x  0  0]
              [:o :x :o]]]    
    
    (is (= (next-board b1 :x [0 0]) b1-x))
    (is (= (next-board b1 :o [0 0]) b1-o))
    (is (nil? (next-board b1 :x [2 2])))))