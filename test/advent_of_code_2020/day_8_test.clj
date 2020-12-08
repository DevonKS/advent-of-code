(ns advent-of-code-2020.day-8-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-8 :refer :all]))

(deftest day-1-part-1-test
  (testing "Sample Data"
    (let [input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
          lines (string/split input #"\n")
          ops (parse-lines lines)]
      (is (= 5
             (challenge-1 ops)))))
  (testing "real-data"
    (is (= 1584
           (challenge-1!)))))

(deftest day-1-part-2-test
  (testing "Sample Data"
    (let [input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
          lines (string/split input #"\n")
          ops (parse-lines lines)]
      (is (= 8
             (challenge-2 ops)))))
  (testing "real-data"
    (is (= 920
           (challenge-2!)))))
