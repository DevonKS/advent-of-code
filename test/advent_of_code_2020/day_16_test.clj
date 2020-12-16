(ns advent-of-code-2020.day-16-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-16 :refer :all]))

(deftest day-16-part-1-test
  (testing "Sample Data 1"
    (let [input-string "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"
          input (parse-input input-string)]
      (is (= 71
             (challenge-1 input)))))
  (testing "real-data"
    (is (= 21978
           (challenge-1!)))))

(deftest day-16-part-2-test
  (testing "real-data"
    (is (= 1053686852011
           (challenge-2!)))))
