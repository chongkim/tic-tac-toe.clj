(ns tic-tac-toe.core
  (:gen-class))

(require '[clojure.string :as string])

(defn square [n] (* n n))

(defrecord Position [board turn dim])

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
     (map->Position {:board (vec (take (square obj) (cycle '[-])))
                     :ply 0
                     :turn 'x
                     :dim obj}))))

(defn position-str [position]
  (string/join "\n-----------\n" 
               (map #(str " " (string/join " | " %) " ")
                    (->>
                      (:board position) 
                      (map-indexed #(if (= '- %2) %1 %2))
                      (partition 3)))))

(defn other-turn [turn-or-position]
  (cond (= turn-or-position 'x) 'o
        (= turn-or-position 'o) 'x
        :else (recur (:turn turn-or-position))))

(defn move [position number]
  (merge position {:turn (other-turn position)
                   :ply (inc (:ply position))
                   :board (assoc (:board position)
                                 number
                                 (:turn position))}))

(defn win-lines [position]
  (let [board (:board position)
        dim (:dim position)
        rows (partition dim board)] 
    (lazy-seq
      (concat rows
              (apply mapv vector rows)
              (vector (keep-indexed
                        #(when (= (rem %1 dim)
                                  (quot %1 dim)) %2)
                        board)
                      (keep-indexed
                        #(when (= (- (dec dim) (rem %1 dim))
                                  (quot %1 dim)) %2)
                        board))))))

(defn win? [position piece]
  (let [line-win? (fn [row] (every? #(= % piece) row)) ]
    (some line-win? (win-lines position))))

(defn blocked? [position]
  (every? #(and (some #{'x} %)
                (some #{'o} %)) (win-lines position)))

(defn possible-moves [position]
  (vec (keep-indexed #(when (= '- %2) %1) (:board position))))

(defn evaluate-leaf [position]
  (cond (win? position 'x) 100
        (win? position 'o) -100
        (blocked? position) 0
        :else nil))

(defn evaluate [position]
  (let [leaf-value (evaluate-leaf position)]
    (if leaf-value
      ((if (neg? leaf-value) + -) leaf-value (:ply position))
      (apply (if (= 'x (:turn position)) max min)
             (map #(evaluate (move position %)) (possible-moves position))))))

(defn best-move [position]
  (let [value-move-pairs (map #(vector (evaluate (move position %)) %)
                              (possible-moves position))
        cmp (if (= 'x (:turn position)) > <)
        max-pair #(if (nil? %1)
                       %2
                       (if (cmp (first %2) (first %1)) %2 %1))]
    (second (reduce max-pair nil value-move-pairs))))

(defn prompt [msg]
  (print msg)
  (flush)
  (read-line))

(defn quit []
  (throw (Throwable. "quit")))

(defn prompt-for-player []
  (println "Who plays first")
  (println)
  (println "1. You")
  (println "2. Computer")
  (println "q. quit")
  (println)
  (loop []
    (let [response (prompt "choose [1,2,q]: ")]
      (cond (= "1" response) (cycle '[you computer])
            (= "2" response) (cycle '[computer you])
            (= "q" response) (quit)
            :else (recur)))))

(defn to-Integer [str]
  "converts to Integer or returns false if formatting problems"
  (try
    (Integer. str)
    (catch NumberFormatException e
      false)))

(defn valid-move? [position number]
  (and (<= 0 number)
       (< number (square (:dim position)))
       (= '- (nth (:board position) number))))

(defn prompt-for-move [position]
  (println (position-str position))
  (loop []
    (let [response (prompt "move [0-8,q]: ")]
      (when (= "q" response) (quit))
      (let [number (to-Integer response)]
        (if (and number
                 (valid-move? position number))
          number
          (recur))))))

(defn prompt-for-replay []
  (let [response (prompt "Do you want to play again? [y,n,q]: ")]
    (cond (= response "y") true
          (= response "n") false
          (= response "q") (quit)
          :else (recur))))

(defn -main
  "Play a game of tic-tac-toe."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Welcome to Tic-Tac-Toe")
  (try
    (loop []
      (loop [position (init-position)
             player-cycle (prompt-for-player)]
        (if (win? position (other-turn position))
          (println "Winner: " (second player-cycle))
          (when-not (= (:ply position)
                       (square (:dim position)))
            (recur (move position (if (= (first player-cycle) 'computer)
                                    (best-move position) 
                                    (prompt-for-move position)))
                   (rest player-cycle)))))
      (when (prompt-for-replay) (recur)))
    (catch Throwable e
      (when-not (= "quit" (.getMessage e))
        (println "error: " (.getMessage e))))) )
