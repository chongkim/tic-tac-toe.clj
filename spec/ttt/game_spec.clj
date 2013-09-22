(ns ttt.game-spec
  (:require [speclj.core :refer :all]
            [ttt.game :refer :all]
            [ttt.position :refer :all]))

(declare ^:dynamic *read-list*)

(defn read-stub []
  (let [read-list @*read-list*]
    (dosync (ref-set *read-list* (rest read-list)))
    (first read-list)))

(describe "ttt.game"
  (context "render"
    (it "should render a position"
      (should= (str " x | x | 2 \n"
                    "-----------\n"
                    " 3 | o | o \n"
                    "-----------\n"
                    " 6 | 7 | 8 \n") (render {:board '[x x -
                                                       - o o
                                                       - - -] :turn 'x}))))
  (context "ask-for-player"
    (it "should quit"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["q"])]
          (should= nil (ask-for-player)))))
    (it "should pick the computer"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["1"])]
          (should= 'computer (ask-for-player)))))
    (it "should pick the human"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["2"])]
          (should= 'human (ask-for-player)))))
    (it "should handle bad input"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["x" "2"])]
          (should= 'human (ask-for-player))))))
  (context "ask-for-move"
    (it "should quit"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["q"])]
          (should= nil (ask-for-move init-position)))))
    (it "should make a valid move"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["xx" "1"])]
          (should= 1 (ask-for-move init-position))))))
  (context "play"
    (it "should quit"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["q"])]
          (should= 'quit (play)))))
    (it "should play a game to a draw"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["2" "0" "1" "6" "5" "7" "q"])]
          (should= 'draw (play)))))
    (it "should play a game to a win for computer"
      (with-redefs [read-line read-stub]
        (binding [*read-list* (ref ["1" "0" "7" "4" "q"])]
          (should= 'computer (play)))))
    
    )
  )

(run-specs)
