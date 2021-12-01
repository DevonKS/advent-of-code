(ns code-advent-2018.day-1.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-1.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= 556 (challenge1 "input.txt")))))

(deftest challenge2-test
  (testing "Challenge 2 Test"
    (is (= 448 (challenge2 "input.txt")))))
