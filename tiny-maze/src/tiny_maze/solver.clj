(ns tiny-maze.solver
  (:require [clojure.set :as s]
            [clojure.pprint :refer :all]
            [tiny-maze.maze :refer :all]
            )
  )


(def m1 [[:S  0  1]
         [ 1  0  1]
         [ 1  0 :E]])


(def m2 [[:S  0  0  1]
         [ 1  1  0  0]
         [ 1  0  0  1]
         [ 1  1  0 :E]])


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


(def w1 (wall-coords m1))
(def w2 (wall-coords m2))


(defn non-visited-open-neighbors
  [maze visited pair]
  (let [nbs (neighbors maze pair)
        walls (wall-coords maze)]
    (s/difference nbs visited walls)))


(defn only-walls-or-visited [maze visited walls pair]
  (let [nvn (non-visited-open-neighbors maze (conj visited pair) pair)]
    (= 0 (count (s/difference nvn walls)))))


(defn viable-neighbors [maze visited pair]
  (let [nvn   (non-visited-open-neighbors maze visited pair)
        walls (wall-coords maze)
        owov  (partial only-walls-or-visited maze (conj visited pair) walls)]
    (->>
      nvn
      (filter #(not (owov %)))
      set)))


(defn contains-end?
  [ps mz]
  ,,,
  )


(defn solve-maze [maze]
  ,,,
  )


#_(
   (load-file "src/tiny_maze/solver.clj") (use 'tiny-maze.solver)
   (load-file "src/tiny_maze/maze.clj") (use 'tiny-maze.maze)
   ; (rows-to-coords m1)
   ; (start-coords m1)
   ; (empty-coords m1)
   ; (wall-coords m1)
   ; (end-coords m1)
   (neighbors m1 [0 0])
   (neighbors m1 [0 1])
   (neighbors m1 [1 1])
   (neighbors m1 [2 1])
   (non-visited-open-neighbors m1 #{} [0 1])
   (non-visited-open-neighbors m1 #{[1 1] [1 2]} [0 1]) ;; #{[0 0] [0 2]}

   ; [[:x :x 1]
   ;  [ 1 :h 1]   ;; :h == here
   ;  [ 1  0 :E]]

   (def ph [1 1]) (def v1 #{[0 0] [0 1]}) (def v1-pp (conj v1 ph)) (def w1 (wall-coords m1))
   (non-visited-open-neighbors m1 v1 [1 1]) ;; #{[1 0] [2 1] [1 2]}
   (only-walls-or-visited m1 v1-pp w1 [1 0])
   (only-walls-or-visited m1 v1-pp w1 [2 1])
   (only-walls-or-visited m1 v1-pp w1 [1 2])
   (viable-neighbors m1 v1 [1 1])

   ;; [[:S :x :x  1]
   ;;  [ 1  1 :x  0]
   ;;  [ 1  0 :h  1]   ;; :h == here
   ;;  [ 1  1  0 :E]]

   (load-file "src/tiny_maze/solver.clj") (use 'tiny-maze.solver)
   (load-file "src/tiny_maze/maze.clj") (use 'tiny-maze.maze)
   (def p2 [2 2]) (def v2 #{[0 0] [0 1] [0 2] [1 2]})
   (def v2-pp (conj v2 p2)) ; (def w2 (wall-coords m2))
   (non-visited-open-neighbors m2 v2 p2) ;; #{[2 1] [3 2]}
   (only-walls-or-visited m2 v2-pp w2 [2 1]) ;; true
   (only-walls-or-visited m2 v2-pp w2 [3 2]) ;; false
   (viable-neighbors m2 v2 p2)
   )
