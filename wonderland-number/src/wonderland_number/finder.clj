(ns wonderland-number.finder)


(def min-number (reduce * (repeat 5 10)))
(def max-number (* 10 min-number))

(defn number-to-digits [n] (set (map int (str n))))
(defn numbers-have-same-digits? [n1 n2]
  (= (number-to-digits n1) (number-to-digits n2)))

(def multipliers #{2 3 4 5 6})

(defn try-multipliers [n]
  (= multipliers
     (set (filter #(numbers-have-same-digits? n (* n %)) multipliers))))

(defn wonderland-condition [n] (if (try-multipliers n) n false))

(defn wonderland-number []
  (some wonderland-condition (range min-number max-number)))

; (use 'wonderland-number.finder)
; (wonderland-number)
