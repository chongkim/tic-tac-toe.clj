(ns ttt.engine
  (:require [ttt.position :refer :all]))

(defn choose [turn x o]
  (if (= 'x turn) x o))

(declare minimax)

(defn store-minimax [{:keys [turn] :as position}]
  (let [moves (possible-moves position)]
    (cond (win? position 'x) 100
          (win? position 'o) -100
          (empty? moves) 0
          :else ((choose turn + -)
                 (apply (choose turn max min)
                        (map #(->> % (move position) minimax) moves))
                 (count moves)))))

(def minimax (memoize store-minimax))

(defn best-move [{:keys [turn] :as position}]
  (let [moves (possible-moves position)]
    (apply (choose turn max-key min-key) #(->> % (move position) minimax) moves)))
