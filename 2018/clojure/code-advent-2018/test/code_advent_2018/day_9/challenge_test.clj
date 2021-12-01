(ns code-advent-2018.day-9.challenge-test
    (:require [clojure.test :refer :all]
              [code-advent-2018.day-9.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= 32 (challenge1 9 25)))
    (is (= 8317 (challenge1 10 1618)))
    (is (= 2764 (challenge1 17 1104)))))
  
(deftest challenge2-test
  (testing "Challenge 1 test"
    (is (= 32 (challenge2 9 25)))
    (is (= 8317 (challenge2 10 1618)))
    (is (= 146373 (challenge2 13 7999)))
    (is (= 2764 (challenge2 17 1104)))
    (is (= 54718 (challenge2 21 6111)))
    (is (= 37305 (challenge2 30 5807)))
    (is (= 404611 (challenge2 431 70950)))
    (is (= 3350093681 (challenge2 431 7095000)))))