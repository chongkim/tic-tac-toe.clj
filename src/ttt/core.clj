(ns ttt.core
  (:require [ttt.game :refer :all]))
(defn main []
  (case (play)
    quit (println "goodbye")
    computer (println "Computer wins!")
    human (println "You won!")
    draw (println "Draw")))
