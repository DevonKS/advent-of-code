(ns advent-of-code-2020.day-1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day-1 :refer :all]))

(deftest day-1-part-1-test
  (testing "Sample Data"
    (is (= 514579
           (challenge-1 [1721 979 366 299 675 1456]))))
  (testing "real-data"
    (is (= 471019
           (challenge-1!)))))

(deftest day-1-part-2-test
  (testing "Sample Data"
    (is (= 241861950
           (challenge-2 [1721 979 366 299 675 1456]))))
  (testing "real-data"
    (is (= 103927824
           (challenge-2!)))))
