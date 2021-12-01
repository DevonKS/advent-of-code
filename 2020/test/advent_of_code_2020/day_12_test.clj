(ns advent-of-code-2020.day-12-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-12 :refer :all]))

(deftest day-12-part-1-test
  (testing "Sample Data"
    (let [input "F10
N3
F7
R90
F11"
          lines (string/split input #"\n")
          ops (parse-lines lines)]
      (is (= 25
             (challenge-1 ops)))))
  (testing "real-data"
    (is (= 1319
           (challenge-1!)))))

(deftest day-12-part-2-test
  (testing "Sample Data"
    (let [input "F10
N3
F7
R90
F11"
          lines (string/split input #"\n")
          seats (parse-lines lines)]
      (is (= 286
             (challenge-2 seats)))))
  (testing "real-data"
    (is (= 62434
           (challenge-2!)))))
