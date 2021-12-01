(ns code-advent-2018.day-3.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-3.challenge :refer :all]))

(deftest parse-claim-test
  (testing "Parse Claim Test"
    (is (= {:id "1" :start-coordinates [829 837] :dimensions [11 22]} (parse-claim "#1 @ 829,837: 11x22")))
    (is (= {:id "34" :start-coordinates [35 270] :dimensions [11 25]} (parse-claim "#34 @ 35,270: 11x25")))))

(deftest get-claim-points-test
  (testing "Get Claim Points Test"
    (is (= [[829, 837], [830, 837], [831, 837]] (get-claim-points {:id "1" :start-coordinates [829 837] :dimensions [3 1]})))
    (is (= [[35, 270], [35, 271], [36, 270], [36, 271]] (get-claim-points {:id "34" :start-coordinates [35 270] :dimensions [2 2]})))))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= 104126 (challenge1 "input.txt")))))

(deftest challenge2-test
  (testing "Challenge 2 Test"
    (is (= "695" (challenge2 "input.txt")))))