(ns tic-tac-toe.core
  (:gen-class))

(require '[clojure.string :as string])

(defrecord Position [board turn dim])

(defn make-vector [n]
  (vec (take n (cycle [nil]))))

(defn init-position
  "create a new position"
  ([] (init-position 3))
  ([obj]
   (cond
     (= clojure.lang.PersistentVector (class obj))
     (map->Position {:board obj
                     :ply 0
                     :turn 'x
                     :dim (long (Math/sqrt (count obj)))}) 
     (= java.lang.Long (class obj))
     (map->Position {:board (make-vector (* obj obj))
                     :ply 0
                     :turn 'x
                     :dim obj}))))

(defn position-str [position]
  (string/join "\n-----------\n" 
               (map #(str " "
                          (string/join " | "
                                       (replace {nil " "} %))
                          " ")
                    (partition 3 (:board position)))))

(defn other-turn [turn-or-position]
  (cond (= turn-or-position 'x) 'o
        (= turn-or-position 'o) 'x
        :else (recur (:turn turn-or-position))))

(defn move [position pos]
  (merge position {:turn (other-turn position)
                   :ply (inc (:ply position))
                   :board (assoc (:board position)
                                 pos
                                 (:turn position))}))

(defn win? [position piece]
  (let [line-win? (fn [row] (every? #(= % piece) row))
        dim (:dim position)
        board (:board position)
        rows (partition dim board)
        columns (apply mapv vector rows)
        major-diagonal (keep-indexed
                         #(when (= (rem %1 dim)
                                   (quot %1 dim)) %2)
                         board)
        minor-diagonal (keep-indexed
                         #(when (= (- (dec dim) (rem %1 dim))
                                   (quot %1 dim)) %2)
                         board)]
    (or (some line-win? rows)
        (some line-win? columns)
        (line-win? major-diagonal)
        (line-win? minor-diagonal))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
