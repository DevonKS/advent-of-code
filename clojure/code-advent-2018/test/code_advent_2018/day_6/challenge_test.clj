(ns code-advent-2018.day-6.challenge-test
    (:require [clojure.test :refer :all]
              [code-advent-2018.day-6.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= 4398 (challenge1 "input.txt")))
    (is (= 17 (challenge1 "test-input.txt")))))


(deftest challenge2-test
  (testing "Challenge 2 Test"
    (is (= 16 (challenge2 "test-input.txt" 32)))
    (is (= 39560 (challenge2 "input.txt" 10000)))))