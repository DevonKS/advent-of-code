(ns code-advent-2018.day-8.challenge-test
    (:require [clojure.test :refer :all]
              [code-advent-2018.day-8.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= 138 (challenge1 "test-input.txt")))
    (is (= 45750 (challenge1 "input.txt")))))

(deftest challenge2-test
    (testing "Challenge 2 Test"
        (is (= 66 (challenge2 "test-input.txt")))
        (is (= 23266 (challenge2 "input.txt")))))