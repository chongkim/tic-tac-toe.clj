(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))

(deftest init-position-test
  (let [position (init-position)
        ply (:ply position)]
    (testing "initialize a board."
      (is (= (:board position) [nil nil nil, nil nil nil, nil nil nil])))
    (testing "make sure it is x's turn"
      (is (= (:turn position) 'x)))))

(deftest position-string-test
  (testing "display of position"
    (is (= (position-str (init-position))
"   |   |   
-----------
   |   |   
-----------
   |   |   "))))

(deftest other-turn-test
  (testing "change turn"
    (is (= 'x (other-turn 'o)))
    (is (= 'o (other-turn 'x)))
    (is (= 'o (other-turn (init-position))))))

(deftest move-test
  (testing "making a move"
    (let [position (init-position)
          new-position (move position 4)]
      (is (= (:board new-position)
             [nil nil nil, nil 'x nil, nil nil nil]))
      (is (= (other-turn position)
             (:turn new-position)))
      (is (= (:ply new-position)
             (inc (:ply position)))))))

(deftest win-test
  (testing "check for no win"
    (is (not (win? (init-position '[x o o, o x x, x x o]) 'x))))
  (testing "check for row 1 win"
    (is (win? (init-position '[x x x, o o x, x o o]) 'x)))
  (testing "check for row 2 win"
    (is (win? (init-position '[o x o, x x x, o o x]) 'x)))
  (testing "check for row 3 win"
    (is (win? (init-position '[x o o, o o x, x x x]) 'x)))
  (testing "check for col 1 win"
    (is (win? (init-position '[x o o, x o x, x x o]) 'x)))
  (testing "check for col 2 win"
    (is (win? (init-position '[o x o, o x x, x x o]) 'x)))
  (testing "check for col 3 win"
    (is (win? (init-position '[o o x, x o x, o x x]) 'x)))
  (testing "check for major diagonal win"
    (is (win? (init-position '[x o o, o x x, o x x]) 'x)))
  (testing "check for minor diagonal win"
    (is (win? (init-position '[o o x, o x x, x x o]) 'x)))
  )

