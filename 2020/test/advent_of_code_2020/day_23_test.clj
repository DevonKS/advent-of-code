(ns advent-of-code-2020.day-23-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day-23 :refer :all]))

(deftest day-23-part-1-test
  (testing "Sample Data"
    (let [cups [3 8 9 1 2 5 4 6 7]]
      (is (= "67384529"
             (challenge-1 cups)))))
  (testing "real-data"
    (is (= "76952348"
           (challenge-1!)))))

(deftest day-23-part-2-test
  ;This test takes too long to run
  #_(testing "Sample Data"
      (let [cups [3 8 9 1 2 5 4 6 7]]
        (is (= 149245887792
               (challenge-1 cups)))))
  (testing "real-data"
    (is (= 72772522064
           (challenge-2!)))))
