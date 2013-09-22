(ns ttt.position-spec
  (:require [speclj.core :refer :all]
            [ttt.position :refer :all]))

(describe "ttt.position"
  (context "init-position"
    (it "should create a new position"
      (should= {:board '[- - -, - - -, - - -] :turn 'x} init-position)))
  (context "move"
    (it "should make a move"
      (should= {:board '[x - -, - - -, - - -] :turn 'o} (move init-position 0))))
  (context "possible-moves"
    (it "should list possible moves for a position"
      (should= [2,3,6,7,8] (possible-moves {:board '[x x -, - o o, - - -]}))))
  (context "win?"
    (it "should determine no win"
      (should-not (win? init-position 'x)))
    (it "should determine a win for x in first row"
      (should (win? {:board '[x x x, - - -, - - -] :turn 'x} 'x)))
    (it "should determine a win for x in second row"
      (should (win? {:board '[- - -, x x x, - - -] :turn 'x} 'x)))
    (it "should determine a win for x in third row"
      (should (win? {:board '[- - -, - - -, x x x] :turn 'x} 'x)))
    (it "should determine a win for o in first col"
      (should (win? {:board '[o - -
                              o - -
                              o - -] :turn 'o} 'o)))
    (it "should determine a win for o in second col"
      (should (win? {:board '[- o -
                              - o -
                              - o -] :turn 'o} 'o)))
    (it "should determine a win for o in third col"
      (should (win? {:board '[- - o
                              - - o
                              - - o] :turn 'o} 'o)))
    (it "should determine a win for o in major diagonal"
      (should (win? {:board '[o - -
                              - o -
                              - - o] :turn 'o} 'o)))
    (it "should determine a win for o in minor diagonal"
      (should (win? {:board '[- - o
                              - o -
                              o - -] :turn 'o} 'o)))
    )
  )

(run-specs)
