(ns advent-of-code-2020.day-18-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-18 :refer :all]))

(deftest day-18-part-1-test
  (testing "Sample Data"
    (let [input-string "1 + (2 * 3) + (4 * (5 + 6))
2 * 3 + (4 * 5)
5 + (8 * 3 + 9 + 3 * 4 * 3)
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
          expressions (string/split-lines input-string)]
      (is (= 26386
             (challenge-1 expressions)))))
  (testing "real-data"
    (is (= 4491283311856
           (challenge-1!)))))

(deftest day-18-part-2-test
  (testing "Sample Data"
    (let [input-string "1 + (2 * 3) + (4 * (5 + 6))
2 * 3 + (4 * 5)
5 + (8 * 3 + 9 + 3 * 4 * 3)
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
          expressions (string/split-lines input-string)]
      (is (= 693942
             (challenge-2 expressions)))))
  (testing "real-data"
    (is (= 68852578641904
           (challenge-2!)))))
