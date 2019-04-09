(ns code-advent-2018.day-12.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-12.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge1 Test"
    (is (= 325 (challenge1 "test-input.txt")))
    (is (= 2063 (challenge1 "input.txt")))))

(deftest challenge2-test
  (testing "Challenge2 Test"
    (is (= 1600000000328 (challenge2 "input.txt")))))