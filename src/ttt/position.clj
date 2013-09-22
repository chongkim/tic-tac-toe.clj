(ns ttt.position)

(def dim 3)
(def size (* dim dim))

(def init-position {:board (vec (replicate size '-)) :turn 'x})

(defn move [{:keys [board turn]} idx]
  {:board (assoc board idx turn) :turn (if (= 'x turn) 'o 'x)})

(defn possible-moves [{:keys [board]}]
  (keep-indexed #(if (= '- %2) %1) board))

(defn win? [{:keys [board]} turn]
  (let [line-match? (fn [line] (every? #(= turn %) line))
        rows (partition dim board)]
    (or
      (some line-match? rows)
      (some line-match? (apply map vector rows))
      (line-match? (map #(nth board %) (range 0 size (inc dim))))
      (line-match? (map #(nth board %) (range (dec dim) (dec size) (dec dim)))))))
