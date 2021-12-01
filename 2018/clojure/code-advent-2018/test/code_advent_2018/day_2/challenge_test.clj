(ns code-advent-2018.day-2.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-2.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= 7163 (challenge1 "input.txt")))))

(deftest test-string-for-likeness-test
  (testing "test-string-for-likeness test"
    (is (= "acd" (test-string-for-likeness "abcd" ["defg" "abbc" "aeed" "ascd"])))))

(deftest challenge2-test
  (testing "Challenge 2 Test"
    (is (= "ighfbyijnoumxjlxevacpwqtr" (challenge2 "input.txt")))))