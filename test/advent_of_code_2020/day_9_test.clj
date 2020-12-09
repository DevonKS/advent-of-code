(ns advent-of-code-2020.day-9-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-9 :refer :all]))

(deftest day-9-part-1-test
  (testing "Sample Data"
    (let [input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"
          lines (string/split input #"\n")
          nums (parse-lines lines)
          preamble 5]
      (is (= 127
             (challenge-1 nums preamble)))))
  (testing "real-data"
    (is (= 104054607
           (challenge-1!)))))

(deftest day-9-part-2-test
  (testing "Sample Data"
    (let [input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"
          lines (string/split input #"\n")
          nums (parse-lines lines)
          preamble 5]
      (is (= 127
             (challenge-1 nums preamble)))))
  (testing "real-data"
    (is (= 13935797
           (challenge-2!)))))
