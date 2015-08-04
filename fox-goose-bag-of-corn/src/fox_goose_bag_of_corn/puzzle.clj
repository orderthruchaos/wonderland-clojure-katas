(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :refer :all]))

; (use 'clojure.set)

(def start-pos [[:fox :goose :corn :you] [:boat] []])
(def end-pos   [[] [:boat] [:fox :goose :corn :you]])

(def bad-pairs #{#{:fox :goose} #{:goose :corn}})
; (def all-goods #{:fox :goose :corn})

; (def
;   boat-okay? (fn [[_ b _]]
;                (let [s (set b)]
;                  (cond
;                    (= #{:boat} s) true
;                    (= #{:boat :you} s) true
;                    (and
;                      (= #{:boat :you} (intersection s #{:boat :you}))
;                      (= 1 (count (difference s #{:boat :you})))) true
;                    :else false)))
;   )
; (boat-okay? start-pos)
; (boat-okay? end-pos)

; (defn move-first [ ps ] (first (apply intersection ps)))
; (move-first bad-pairs)


; (def left-shore  (fn [[v _ _]] v))
; (def in-boat     (fn [[_ v _]] v))
; (def right-shore (fn [[_ _ v]] v))

(def pos-to-sets (fn [p] (apply vector (map set p))))
(def sets-to-pos (fn [p] (apply vector (map (partial apply vector) p))))

; (def plan-to-sets (fn [p] (apply vector (map pos-to-sets p))))
(def sets-to-plan (fn [p] (apply vector (map sets-to-pos p))))

; (def
;   good-move? (fn [m]
;                (let [[l _ r] (pos-to-sets m)]
;                  (if (empty? (intersection #{l r} bad-pairs))
;                    m
;                    false)))
;   )

(def
  move-set-left (fn [[l m r] s]
                  (let [new_r (difference r s)
                        new_l (union      l s)
                        tmp_m (union      m s)]
                    [[l tmp_m new_r] [new_l m new_r]]
                    ))
  )

(def
  move-set-right (fn [[l m r] s]
                   (let [new_l (difference l s)
                         new_r (union      r s)
                         tmp_m (union      m s)]
                     [[new_l tmp_m r] [new_l m new_r]]))
  )

(def
  move-item (fn [pos i]
              (let [[l _ r :as pos-sets] (pos-to-sets pos)
                    src (if (l :you) l r)
                    dst (if (l :you) r l)
                    ; s (if (<= (count r) 2) #{:you} #{:you i})]
                    s (if (nil? i) #{:you} #{:you i})]
                (if (l :you)
                  (move-set-right pos-sets s)
                  (move-set-left  pos-sets s)
                  )
                )
              )
  )


; (def
;   without-you (fn [s] (difference (set s) #{:you}))
;   )


; (def
;   okay-to-move? (fn [pos item]
;                   (let [[l _ r :as pos-sets] (pos-to-sets pos)
;                         s #{:you item}
;                         nl (difference l s)
;                         nr (difference r s)
;                         in (intersection #{nl nr} bad-pairs)]
;                     (if (empty? in) item false)))
;   )

; (def
;   okay-to-swap? (fn [pos l-item r-item]
;                   (let [[l _ r :as pos-sets] (pos-to-sets pos)
;                         y #{:you}
;                         nl (difference l y)
;                         nr (difference r y)
;                         ls #{l-item}
;                         rs #{r-item}
;                         dl (difference nl ls)
;                         dr (difference nr rs)
;                         ck (set [
;                                  (difference nl ls)
;                                  (difference nr rs)
;                                  (union dl nr)
;                                  ])
;                         in (intersection ck bad-pairs)]
;                     ; (pprint {:l l :r r :y y :nl nl :nr nr :ls ls :rs rs :ck ck :dl dl :dr dr})
;                     (if (empty? in) [l-item r-item] false)))
;   )

; (def p3 [[:fox :corn :you] [:boat] [:goose]])
; (okay-to-swap? p3 :fox :goose)
; (okay-to-swap? p3 :corn :goose)

; (def plan-to-sets (partial map pos-to-sets))
; (def sets-to-plan (partial map sets-to-pos))

(def wanted-river-crossing-plan-1
  [start-pos

   ; [ i1     i2    i3 ]
   ; [ :goose :corn :fox ]

   ; Move goose
   ; Move i1 -> r
   [[:fox :corn] [:boat :goose :you] []]
   [[:fox :corn] [:boat] [:goose :you]]
   ; Move nil -> l
   [[:fox :corn] [:boat :you] [:goose]]

   ; Swap corn for goose
   ; Move i2 -> r
   [[:fox :corn :you] [:boat] [:goose]]
   [[:fox] [:boat :corn :you] [:goose]]
   ; Move i1 -> l
   [[:fox] [:boat] [:goose :corn :you]]
   [[:fox] [:boat :goose :you] [:corn]]
   [[:fox :goose :you] [:boat] [:corn]]

   ; Move fox
   ; Move i3 -> r
   [[:goose] [:boat :fox :you] [:corn]]
   [[:goose] [:boat] [:corn :fox :you]]
   ; Move nil -> l
   [[:goose] [:boat :you] [:corn :fox]]

   ; Move goose
   ; Move i1 -> r
   [[:goose :you] [:boat] [:corn :fox]]
   [[] [:boat :goose :you] [:corn :fox]]
   end-pos])

(def wanted-river-crossing-plan-2
  [start-pos

   ; [ i1     i2    i3 ]
   ; [ :goose :corn :fox ]

   ; Move goose
   ; Move i1 -> r
   [[:fox :corn] [:boat :goose :you] []]
   [[:fox :corn] [:boat] [:goose :you]]
   ; Move nil -> l
   [[:fox :corn] [:boat :you] [:goose]]

   ; Swap fox for goose
   ; Move i2 -> r
   [[:fox :corn :you] [:boat] [:goose]]
   [[:corn] [:boat :fox :you] [:goose]]
   ; Move i1 -> l
   [[:corn] [:boat] [:goose :fox :you]]
   [[:corn] [:boat :goose :you] [:fox]]
   [[:corn :goose :you] [:boat] [:fox]]

   ; Move corn
   ; Move i3 -> r
   [[:goose] [:boat :corn :you] [:fox]]
   [[:goose] [:boat] [:fox :corn :you]]
   ; Move nil -> l
   [[:goose] [:boat :you] [:fox :corn]]

   ; Move goose
   ; Move i1 -> r
   [[:goose :you] [:boat] [:corn :fox]]
   [[] [:boat :goose :you] [:corn :fox]]
   end-pos])



(defn river-crossing-plan []
  (let [ci (first (apply intersection bad-pairs))
        rm (difference (apply union bad-pairs) #{ci})
        i1 (first rm)
        i2 (last  rm)]
    {:ci ci :rm rm :i1 i1 :i2 i2}
    (loop [ps [(pos-to-sets start-pos)]
           ms [ci nil i1 ci i2 nil ci]]
      (if (empty? ms)
        (sets-to-plan ps)
        (recur (concat ps (move-item (last ps) (first ms))) (rest ms))))))
; (pprint (river-crossing-plan))

(defn perms
  [[:fox :corn :goose]
   [:fox :goose :corn]
   [:corn :fox :goose]
   [:corn :goose :fox]
   [:goose :fox :corn]
   [:goose :corn :fox]])
