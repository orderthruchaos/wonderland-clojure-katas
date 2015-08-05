(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.set :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string]))

; (load-file "src/doublets/solver.clj") (use 'doublets.solver)

(def ^:private words (-> "words.edn"
                         (io/resource)
                         (slurp)
                         (read-string)))

(defn- words-of-correct-length [w]
  (let [l (count w)]
    (sort (filter #(= l (count %)) words))))

(defn- empty-node [t] {:word   nil
                      :found  []
                      :regex  nil
                      :target t
                      :list   (words-of-correct-length t)})

(defn- solver-branch? [n]
  (not (and (map? n) (= (:target n) (get n :word)))))

(defn- word-to-regex-part [w i _]
  (apply str (concat (take i w) "." (drop (inc i) w))))

(defn- word-to-regex [w]
  (re-pattern
    (str "^("
         (clojure.string/join
           "|"
           (map-indexed (partial word-to-regex-part w) w))
         ")$"))
  )

(defn- word-to-node [w m]
  (-> m
      (assoc :word w)
      (assoc :found (conj (get m :found []) w))
      (assoc :regex (word-to-regex w))
      )
  )

(defn- solver-children [n]
  (let [re (get n :regex)
        ws (set (filter #(re-matches re %) (:list n)))]
    (map #(word-to-node % n) (sort (difference ws (set (get n :found [])))))))

(defn- solver-tree [w1 w2] (tree-seq solver-branch?
                                    solver-children
                                    (word-to-node w1 (empty-node w2))))

(defn- solutions [w1 w2]
  (let [t (solver-tree w1 w2)]
    (sort-by count (map :found (filter #(= w2 (last (get % :found))) t)))))

(defn doublets [word1 word2]
  (let [l1 (count word1)
        l2 (count word2)]
    (if (= l1 l2)
      (first (solutions word1 word2))
      [])
    )
  )

