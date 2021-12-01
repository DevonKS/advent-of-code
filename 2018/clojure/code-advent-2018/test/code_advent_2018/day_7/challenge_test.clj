
(ns code-advent-2018.day-7.challenge-test
    (:require [clojure.test :refer :all]
              [code-advent-2018.day-7.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= "CABDFE" (challenge1 "test-input.txt")))
    (is (= "BITRAQVSGUWKXYHMZPOCDLJNFE" (challenge1 "input.txt")))))

(deftest challenge2-test
    (testing "Challenge 2 Test"
        (is (= 258 (challenge2 "test-input.txt" 2)))
        (is (= 869 (challenge2 "input.txt" 5)))))