(ns tiny-maze.solver)


(def m1 [[:S 0  1]
         [ 1 0  1]
         [ 1 0 :E]])


(defn cols-to-coords [cs i] (map-indexed (fn [j c] [[i j] c]) cs)) 


(defn rows-to-coords [rs]
  (->>
    (map-indexed (fn [i cs] (cols-to-coords cs i)) rs)
    (apply concat)
    (into {})
    )
  )


(defn coords-with-value [v maze]
  (->>
    maze
    rows-to-coords
    (filter #(= v (second %)))
    (map first)
    set
    )
  )


(defn maze-size [maze]
  (let [m (count maze)
        n (count (first maze))]
    [m n]))


(defn neighbors [maze [x y]]
  (let [[m n] (maze-size maze)]
    (->>
      [[(dec x) y]
       [(inc x) y]
       [x (dec y)]
       [x (inc y)]]
      (filter #(and (< -1 (first %) m) (< -1 (last %) n)))
      set)))


(defn start-coords [maze] (first (coords-with-value :S maze)))
(defn end-coords   [maze] (first (coords-with-value :E maze)))
(defn wall-coords  [maze] (coords-with-value  1 maze))
(defn empty-coords [maze] (conj (coords-with-value  0 maze)
                                (end-coords maze)))


(defn solve-maze [maze]
  ,,,)

#_(
   (load-file "src/tiny_maze/solver.clj") (use 'tiny-maze.solver)
   (rows-to-coords m1)
   (start-coords m1)
   (empty-coords m1)
   (wall-coords m1)
   (end-coords m1)
   (neighbors m1 [0 0])
   (neighbors m1 [0 1])
   (neighbors m1 [1 1])
   (neighbors m1 [2 1])
   )
