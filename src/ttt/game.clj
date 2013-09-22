(ns ttt.game
  (:require [clojure.string :refer [join]]
            [ttt.engine :refer :all]
            [ttt.position :refer :all]))

(defn render [{:keys [board]}]
  (str (->> board
            (map-indexed #(if (= '- %2) (format " %x " %1) (format " %s " %2)))
            (partition dim)
            (map #(join "|" %))
            (interpose (apply str (replicate (dec (* 4 dim)) "-")))
            (join "\n"))
       "\n"))

(defn ask-for-player []
  (println "Who do you want to go first?")
  (println "  1. Computer")
  (println "  2. Human")
  (println "  q - quit")
  (let [ans (read-line)]
    (case ans
      "1" 'computer
      "2" 'human
      "q" nil
      (recur))))

(defn ask-for-move [{:keys [board] :as position}]
  (println "Make a move")
  (let [ans (read-line)]
    (cond (= "q" ans) 'nil
          (re-matches #"^[\da-f]" ans)
          (let [idx (Integer/parseInt ans)]
            (if (and (contains? board idx)
                     (= '- (nth board idx)))
              idx
              (do
                (println "Pick an empty square on the board")
                (recur position))))
          :else (do
                  (println "Invalid move")
                  (recur position)))))

(defn other [player]
  (if (= 'human player) 'computer 'human))

(defn game-loop [player {:keys [turn] :as position}]
  (println (render position))
  (cond (or (win? position 'x)
            (win? position 'o)) (other player)
        (empty? (possible-moves position)) 'draw
        :else (let [idx (if (= 'computer player)
                          (best-move position)
                          (ask-for-move position))]
                (if (nil? idx)
                  'quit
                  (recur (other player) (move position idx))))))

(defn play []
  (let [player (ask-for-player)]
    (if (nil? player)
      'quit
      (game-loop player init-position)
      )
    )
  )
