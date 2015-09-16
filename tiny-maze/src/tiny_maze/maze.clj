(ns tiny-maze.maze
  (:require [clojure.set :as s]
            [clojure.pprint :refer :all]
            )
  )


(defn- cols-to-coords [cs i] (map-indexed (fn [j c] [[i j] c]) cs))


(defn- rows-to-coords [rs]
  (->>
    (map-indexed (fn [i cs] (cols-to-coords cs i)) rs)
    (apply concat)
    (into {})
    )
  )


(defn- coords-with-value [v maze]
  (->>
    maze
    rows-to-coords
    (filter #(= v (second %)))
    (map first)
    set
    )
  )


(defn- maze-size [maze]
  (let [m (count maze)
        n (count (first maze))]
    [m n]))



(defn- start-coords [maze] (first (coords-with-value :S maze)))
(defn- end-coords   [maze] (first (coords-with-value :E maze)))
(defn- wall-coords  [maze] (coords-with-value  1 maze))
(defn- empty-coords [maze] (conj (coords-with-value  0 maze)
                                (end-coords maze)))


(defrecord Maze
  [maze    ;; Maze itself
   size    ;; Dimensions of maze
   start   ;; Start point
   end     ;; End point
   walls   ;; Wall coordinates
   open    ;; Open coordinates
   visited ;; Set of visited coordinates
   ])


(defn create-maze [mz]
  (map->Maze {:maze    mz,
              :size    (maze-size mz),
              :start   (start-coords mz),
              :end     (end-coords mz),
              :walls   (wall-coords mz),
              :open    (empty-coords mz),
              :visited #{},
              }))

#_(
   (load-file "src/tiny_maze/maze.clj") (use 'tiny-maze.maze)

   (def mz1 (create-maze [[:S  0  1]
                          [ 1  0  1]
                          [ 1  0 :E]]))


   (def mz2 (create-maze [[:S  0  0  1]
                          [ 1  1  0  0]
                          [ 1  0  0  1]
                          [ 1  1  0 :E]]))

   )
