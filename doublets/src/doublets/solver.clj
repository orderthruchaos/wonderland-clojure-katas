(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string]))

; (load-file "src/doublets/solver.clj")
; (use 'doublets.solver)

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn word-to-regex-part [w i _]
  (apply str (concat (take i w) "." (drop (inc i) w))))

(defn word-to-regex [w]
  (re-pattern
    (str "^("
         (clojure.string/join
           "|"
           (map-indexed (partial word-to-regex-part w) w))
         ")$"))
  )

(defn words-of-correct-length [w]
  (let [l (count w)]
    (sort (filter #(= l (count %)) words))))

(def print-words #(println words))

(defn doublets [word1 word2]
  "make me work")
