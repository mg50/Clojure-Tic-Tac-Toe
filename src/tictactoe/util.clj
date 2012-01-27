(ns tictactoe.util)

(defmacro or-non-empty [& clauses]
  "Functions like the 'or' macro, but treats non-empty values as false. Should only be supplied seqs or
  nil values. Removes nil values from seqs automatically."
  (let [clauses* (map (fn [clause] 
                        `(let [i# (remove nil? ~clause)]
                           (when-not (empty? i#) i#)))
                      clauses)]
    `(or ~@clauses*)))

(defn map-matrix [matrix f]
  "Takes a two-dimensional vector and maps it to a new two-dimensional vector based on the coordinates of
  each cell together with the value of that cell. Thus, f takes three arguments: the cell-value, x and y.
  Borrowed from Alex Yukashev's guide to writing Tetris in Clojure."
  (into [] (map-indexed (fn [y row]
                          (into [] (map-indexed (fn [x el]
                                                  (f el x y))
                                                row)))
                        matrix)))

(defmacro with-accessors [record [& accessors] & body]
  "Automatically binds specified accessors of a record via a let clause. For example, if 'game' is a record,
  with a 'board' field, instead of typing (let [board (:board game)] ~~~), we could write
  (with-accessors game [board] ~~~~)."
  (let [bindings (map (fn [acc]
                        `(~acc (~(keyword acc) ~record)))
                      accessors)]
    `(let [~@(apply concat bindings)] ~@body)))