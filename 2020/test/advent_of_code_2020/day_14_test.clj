(ns advent-of-code-2020.day-14-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-14 :refer :all]))

(deftest day-14-part-1-test
  (testing "Sample Data"
    (let [input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"
          lines (string/split input #"\n")
          ops (parse-lines lines)]
      (is (= 165
             (challenge-1 ops)))))
  (testing "real-data"
    (is (= 9615006043476
           (challenge-1!)))))

(deftest day-14-part-2-test
  (testing "Sample Data"
    (let [input "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"
          lines (string/split input #"\n")
          ops (parse-lines lines)]
      (is (= 208
             (challenge-2 ops)))))
  (testing "real-data"
    (is (= 4275496544925
           (challenge-2!)))))
