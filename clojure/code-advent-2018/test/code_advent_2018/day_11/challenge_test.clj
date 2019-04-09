(ns code-advent-2018.day-11.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-11.challenge :refer :all]))

(deftest find-power-level-test
  (testing "Find Power Level Test"
    (is (= 4 (find-power-level 8 [3 5])))
    (is (= -5 (find-power-level 57 [122 79])))
    (is (= 0 (find-power-level 39 [217 196])))
    (is (= 4 (find-power-level 71 [101 153])))))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= [33 45] (challenge1 18)))
    (is (= [21 68] (challenge1 2568)))))