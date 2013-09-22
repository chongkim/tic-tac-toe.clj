(ns ttt.engine-spec
  (:require [speclj.core :refer :all]
            [ttt.engine :refer :all]
            [ttt.position :refer :all]))

(describe "ttt.engine"
  (context "minimax"
    (it "should determine a win for x"
      (should= 100 (minimax {:board '[x x x, - - -, - - -] :turn 'x})))
    (it "should determine a win for o"
      (should= -100 (minimax {:board '[o o o, - - -, - - -] :turn 'o})))
    (it "should determine a draw"
      (should= 0 (minimax {:board '[o x o, x o x, x o x] :turn 'o})))
    (it "should determine a win for x in 1 move"
      (should= 107 (minimax {:board '[x x -, - - -, - - -] :turn 'x})))
    (it "should determine a win for o in 1 move"
      (should= -107 (minimax {:board '[o o -, - - -, - - -] :turn 'o})))
    (it "should determine a value from cache"
      (should= 5 (minimax init-position))))
  (context "best-move"
    (it "should determine the best move for x"
      (should= 2 (best-move {:board '[x x -, - o o, - - -] :turn 'x})))
    (it "should determine the best move for x"
      (should= 3 (best-move {:board '[x x -, - o o, - - -] :turn 'o}))))
  )

(run-specs)
