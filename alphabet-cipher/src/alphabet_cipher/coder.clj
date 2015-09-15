(ns alphabet-cipher.coder)

(defn- rot-by [n c]
  (let [v   (into [] c)
        len (count v)
        nr  (mod n len)]
    (concat (drop nr v) (take nr v))))

(def all-caps (map char (range (int \A) (inc (int \Z)))))

(defn- shift-for [l] (- (int l) (int \A)))

(defn-
  rot-map [n c]
  (if (char? n)
    (recur (shift-for n) c)
    (into {} (map (fn [v1 v2] [v1 v2]) c (rot-by n c)))))

(def encode-map (into {} (map (fn [l] [l (rot-map l all-caps)]) all-caps)))

(def decode-map
  (into {}
        (map (fn [[k v]]
               [k 
                (into {} (for [[k1 v1] v] [v1 k1] ))
                ]) encode-map)))

(defn-
  phrase-to-seq [s]
  (->> s
       (clojure.string/upper-case)
       (filter #(<= (int \A) (int %) (int \Z)))))

(defn- cypher-with-map
  [msg, kwd, m]
  (let [msgs (phrase-to-seq msg)
        rptc (inc (quot (count msg) (count kwd)))
        kwds (take
               (count msgs)
               (phrase-to-seq (apply str (repeat rptc kwd))))]
    (clojure.string/lower-case
      (apply str (map #(get (get m %2) %1) msgs kwds)))))

(def encode (fn [kwd msg] (cypher-with-map msg kwd encode-map)))

(def decode (fn [kwd msg] (cypher-with-map msg kwd decode-map)))

(defn decypher [cypher message]
  (let [repeating (decode message cypher)
        n         (count repeating)]
    (->> (range 1 (inc n))
         (map #(apply str (take % repeating)))
         (filter #(= cypher (encode % message)))
         first
         )
    )
  )

#_(
   (load-file "src/alphabet_cipher/coder.clj")(use 'alphabet-cipher.coder)
   (decypher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")
   )
