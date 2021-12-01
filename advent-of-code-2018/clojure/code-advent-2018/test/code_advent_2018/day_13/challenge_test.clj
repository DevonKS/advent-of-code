(ns code-advent-2018.day-13.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-13.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge1 Test"
    (is (= [7 3] (challenge1 "test-input.txt")))
    (is (= [116 10] (challenge1 "input.txt")))))

(deftest challenge2-test
  (testing "Challenge2 Test"
    (is (= [116 25] (challenge2 "input.txt")))))