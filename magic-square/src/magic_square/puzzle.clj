(ns magic-square.puzzle
  (:require [clojure.pprint :refer :all]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- remove-n [n vs] (into (empty vs) (remove #(= n %) vs)))

(def ^:private initial-node {:current []
                             :remaining values})

(defn create-children [{r :remaining
                        c :current
                        :as s}]
  (map (fn [v] (assoc (assoc s :current (conj c v))
                      :remaining (remove-n v r))) r))

(defn rows-add-up? [rows]
  (set (map (fn [r] (reduce + r)) rows)))

(defn cols-add-up? [rows]
  (set
    [(reduce + (map first rows))
     (reduce + (map second rows))
     (reduce + (map last rows))]))

(defn diag-add-up? [rows]
  (let [m (second (second rows))]
    (set [(+ (first (first rows)) m (last (last rows)))
          (+ (first (last rows)) m (last (first rows)))])))

(defn all-add-up? [rows]
  (= 1 (count (set (concat
                     (rows-add-up? rows)
                     (cols-add-up? rows)
                     (diag-add-up? rows))))))

(defn filter-combos [vs]
  (filter all-add-up? vs))

(defn combo-has-children? [c]
  ; (pprint c)
  (not (empty? (:remaining c))))

(defn combos [vs]
  (let [n (count vs)]
    (->> initial-node
         (tree-seq combo-has-children? create-children)
         (map :current)
         (filter #(= n (count %)))
         (map (partial partition 3))
         (filter-combos)
         (first)
         )))

(defn magic-square [values]
  (into [] (map (partial apply vector) (combos values))))
