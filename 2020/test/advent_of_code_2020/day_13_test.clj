(ns advent-of-code-2020.day-13-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-13 :refer :all]))

(deftest day-13-part-1-test
  (testing "Sample Data"
    (let [input "939
7,13,x,x,59,x,31,19"
          lines (string/split input #"\n")
          input (parse-lines lines)]
      (is (= 295
             (challenge-1 input)))))
  (testing "real-data"
    (is (= 4808
           (challenge-1!)))))

(deftest day-13-part-2-test
  (testing "Sample Data"
    (let [input "939
7,13,x,x,59,x,31,19"
          lines (string/split input #"\n")
          input (parse-lines lines)]
      (is (= 1068781
             (challenge-2 input)))))
  (testing "Sample Data 2"
    (let [input "\n17,x,13,19"
          lines (string/split input #"\n")
          input (parse-lines lines)]
      (is (= 3417
             (challenge-2 input)))))
  (testing "Sample Data 3"
    (let [input "\n67,7,59,61"
          lines (string/split input #"\n")
          input (parse-lines lines)]
      (is (= 754018
             (challenge-2 input)))))
  (testing "Sample Data 4"
    (let [input "\n67,x,7,59,61"
          lines (string/split input #"\n")
          input (parse-lines lines)]
      (is (= 779210
             (challenge-2 input)))))
  (testing "Sample Data 5"
    (let [input "\n67,7,x,59,61"
          lines (string/split input #"\n")
          input (parse-lines lines)]
      (is (= 1261476
             (challenge-2 input)))))
  (testing "Sample Data 6"
    (let [input "\n1789,37,47,1889"
          lines (string/split input #"\n")
          input (parse-lines lines)]
      (is (= 1202161486
             (challenge-2 input)))))
  (testing "real-data"
    (is (= 741745043105674
           (challenge-2!)))))
